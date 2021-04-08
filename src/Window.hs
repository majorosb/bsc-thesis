{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Window
        where   
import qualified Graphics.Vty as Vty
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified System.Directory as Dir

import Object 
import Data.List
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Border (border)
import Control.Lens
import Control.Monad.IO.Class
-- Todo: handle exceptions when changing dirs

data Name = TabName String | Workspace String | EditName String | WindowName Int | BrowserName String 
        deriving (Eq,Ord,Show)

data Window =
        Window { _currentDir      :: FilePath
               , _objects         :: List Name Object
                                  -- ^ This module provides a scrollable list type and functions for manipulating and rendering it. 
               , _windowName      :: Name
               , _windowException :: Maybe E.IOException
               } 
        
makeLenses ''Window

instance Eq Window where
        (==) w1 w2 = w1^.windowName == w2^.windowName 

newWindow :: Name -> FilePath  ->  IO Window
newWindow resourceName dir = do
     objectStrings <- getFiles dir
     let brickList = list resourceName (V.fromList . sort $ objectStrings) 1
     return $ Window dir brickList resourceName Nothing

refreshWindow :: Window -> IO Window
refreshWindow w = do 
     objectStrings <- getFiles (w^.currentDir)
     let brickList = list (w^.windowName) (V.fromList . sort $ objectStrings) 1
     return $ w & objects.~brickList

changeDir ::  FilePath -> Window -> IO Window  
changeDir newDir window = E.catch go (raiseExceptionInWindow window)
        where
         go = do
           dirExists <- Dir.doesDirectoryExist newDir
           if dirExists then do
             Dir.setCurrentDirectory newDir 
             currDir    <- Dir.getCurrentDirectory 
             newObjects <- getFiles currDir
             let brickList = list (window^.windowName) (V.fromList . sort $ newObjects ) 1
             return $ 
               window & currentDir .~ newDir &
               objects .~ brickList & windowException .~ Nothing
           else return $ window

addIsSelected :: Object -> String
addIsSelected object = if object^.isSelected then "+ " else ""

renderObject :: Bool -> Object -> Widget Name
renderObject _ object = case object^.filetype of
     Directory -> padRight Max (str $  addIsSelected object ++ object^.name ++ "/")
     _         -> padRight Max (str $  addIsSelected object ++ object^.name) 
                 <+> padLeft Max (str $ fileInfo)
        where
                fileInfo = case object^.info of
                             Just inf -> show inf
                             Nothing  -> "N/A"

raiseExceptionInWindow :: Window -> IOError -> IO Window
raiseExceptionInWindow window e = return $ window & windowException .~ (Just e)

renderWindow ::  Window -> Widget Name
renderWindow window =   vBox [joinBorders $ renderList renderObject True (window^.objects) ]

handleWindowEvent ::  Vty.Event -> Window -> EventM Name Window
handleWindowEvent event window =  case event of
     Vty.EvKey (Vty.KChar 'l')  []  -> handleChangeDirForward window 
     Vty.EvKey (Vty.KChar 'h')  []  -> handleChangeDirBackward window
     Vty.EvKey (Vty.KChar 's')  []  -> handleSelect window
     _                              -> do
         h <- handleListEventVi (handleListEvent) event (window^.objects) -- for characters j k g G
         return $ set (objects) h window

handleSelect ::  Window -> EventM Name Window 
handleSelect window = case listSelectedElement (window^.objects) of 
                        Just (_,obj) -> return $ window & objects.~(listModify selectObject (window^.objects))
                        Nothing      -> return window 


handleChangeDirForward ::  Window -> EventM Name Window
handleChangeDirForward window = case listSelectedElement (window^.objects) of
     Just (_ , obj) -> case obj^.filetype of 
              Directory -> liftIO $ changeDir (obj^.path) window
              _         -> return window
     Nothing        -> return window 

handleChangeDirBackward ::  Window -> EventM Name Window
handleChangeDirBackward = liftIO . changeDir ".."
 
selectObject :: Object -> Object
selectObject object = if object^.isSelected 
                         then object & isSelected.~False 
                         else object & isSelected.~True
