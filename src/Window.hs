{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Window
        where   
import qualified Graphics.Vty as Vty
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Brick.AttrMap as A

import Object 
import Data.List
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Border (border)
import Control.Lens
import Control.Monad.IO.Class
import System.Directory 
import System.Environment
import System.FilePath

data Name = TabName String | Workspace String | EditName String | WindowName Int | BrowserName String 
        deriving (Eq,Ord,Show)

data Window =
        Window { _currentDir      :: FilePath
               , _objects         :: List Name Object
                                  -- ^ This module provides a scrollable list type and functions for manipulating and rendering it. 
               , _windowName      :: Name
               , _windowException :: Maybe E.IOException
               , _help            :: Maybe (List Name String)
               } 
        
makeLenses ''Window

instance Eq Window where
        (==) w1 w2 = w1^.windowName == w2^.windowName 

newWindow :: Name -> FilePath -> IO Window
newWindow resourceName dir = do
     dirExists <- doesDirectoryExist dir
     if dirExists 
     then do
             absDir <- makeAbsolute dir
             objectStrings <- getFiles dir
             let brickList = list resourceName (V.fromList . sort $ objectStrings) 1
             return $ Window absDir brickList resourceName Nothing Nothing
     else do
             e <- lookupEnv "HOME" 
             case e of
              Nothing -> do 
               objectStrings <- getFiles "/"
               let brickList = list resourceName (V.fromList . sort $ objectStrings) 1
               return $ Window "/" brickList resourceName Nothing Nothing
              Just home -> do 
               absDir <- makeAbsolute dir
               objectStrings <- getFiles dir
               let brickList = list resourceName (V.fromList . sort $ objectStrings) 1
               return $ Window absDir brickList resourceName Nothing Nothing

                  

refreshWindow :: Window -> IO Window
refreshWindow w = do 
     objectStrings <- getFiles (w^.currentDir)
     let brickList = list (w^.windowName) (V.fromList . sort $ objectStrings) 1
     return $ w & objects.~brickList

changeDir ::  FilePath -> Window -> IO Window  
changeDir newDir window = E.catch go (raiseExceptionInWindow window)
        where
         go = do
           dirExists <- doesDirectoryExist newDir
           if dirExists then do
             setCurrentDirectory newDir 
             currDir    <- getCurrentDirectory 
             newObjects <- getFiles currDir
             let brickList = list (window^.windowName) (V.fromList . sort $ newObjects ) 1
             return $ 
               window & currentDir.~currDir &
               objects.~brickList & windowException.~Nothing
           else return $ window 

addIsSelected :: Object -> String
addIsSelected object = if object^.isSelected then "+ " else ""

renderObject :: Bool -> Object -> Widget Name
renderObject True object  = case object^.filetype of
     Directory -> padRight Max (str $  addIsSelected object ++ object^.name ++ "/")
     _         -> padRight Max (str $  addIsSelected object ++ object^.name) 
                 <+> padLeft Max (str $ fileInfo)
  where
      fileInfo = case object^.info of
          Just inf -> show inf
          Nothing  -> "N/A"
renderObject False object = case object^.filetype of
     Directory -> padRight Max (withAttr (attrib fselect ftype) $ str $ addIsSelected object ++ object^.name ++ "/")
     _         -> padRight Max (withAttr (attrib fselect ftype) $ str $ addIsSelected object ++ object^.name) 
                 <+> padLeft Max (str $ fileInfo)
 where
     ftype = object^.filetype
     fselect = object^.isSelected
     fileInfo = case object^.info of
         Just inf -> show inf
         Nothing  -> "N/A"

attrib :: Bool -> FileType -> A.AttrName
attrib selected f = if selected 
                      then attrSelected
                      else case f of
                        Directory -> attrDir
                        _         -> attrFile

raiseExceptionInWindow :: Window -> IOError -> IO Window
raiseExceptionInWindow window e = return $ window & windowException .~ (Just e)

renderWindow :: Bool ->  Window -> Widget Name
renderWindow b window = case window^.help of
                          Nothing  -> vBox [joinBorders $ renderList renderObject b (window^.objects) ]
                          Just a   -> vBox [joinBorders $ renderList (\b n -> str $ n) b a]

handleWindowEvent ::  Vty.Event -> Window -> EventM Name Window
handleWindowEvent event window =  case event of
     Vty.EvKey (Vty.KChar 'l')  []  -> handleChangeDirForward window 
     Vty.EvKey (Vty.KChar 'h')  []  -> handleChangeDirBackward window
     Vty.EvKey (Vty.KChar 's')  []  -> handleSelect window
     _                              -> do
         h <- handleListEventVi (handleListEvent) event (window^.objects) -- for characters j k g G
         return $ set (objects) h window
handleWindowHelpEvent ::  Vty.Event -> Window -> EventM Name Window
handleWindowHelpEvent event window =  case window^.help of
     Nothing -> return window
     Just a  -> do
         h <- handleListEventVi (handleListEvent) event a -- for characters j k g G
         return $ window & help ?~ h

handleSelect ::  Window -> EventM Name Window 
handleSelect w = case listSelectedElement (w^.objects) of 
                        Just (_,obj) -> return $ w & objects.~(listModify selectObject (w^.objects))
                        Nothing      -> return w

handleChangeDirForward ::  Window -> EventM Name Window
handleChangeDirForward w = case listSelectedElement (w^.objects) of
     Just (_ , obj) -> case obj^.filetype of 
              Directory -> liftIO $ changeDir (obj^.path) w
              _         -> return w
     Nothing            -> return w

handleChangeDirBackward ::  Window -> EventM Name Window
handleChangeDirBackward w = do
        w' <- liftIO . changeDir ((w^.currentDir) </> "..") $ w
        let childDir = dropTrailingPathSeparator (w^.currentDir)
        return w' 
 
selectObject :: Object -> Object
selectObject object = if object^.isSelected 
                         then object & isSelected.~False 
                         else object & isSelected.~True


