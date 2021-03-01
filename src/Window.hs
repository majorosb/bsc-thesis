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
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Control.Lens
import Control.Monad.IO.Class
-- Todo: handle exceptions when changing dirs

data Window a =
        Window { _currentDir :: FilePath
               , _objects :: List a Object
                          -- ^ This module provides a scrollable list type and functions for manipulating and rendering it. 
               , _windowName :: a
               , _windowException :: Maybe E.IOException
               } 
makeLenses ''Window

newWindow :: a -> FilePath  ->  IO (Window a)
newWindow resourceName dir = do
     objectStrings <- getFiles dir
     let brickList = list resourceName (V.fromList objectStrings) 1
     return $ Window dir brickList resourceName Nothing

changeDir ::  FilePath -> Window a -> IO (Window a) 
changeDir newDir window = E.catch go (raiseExceptionInWindow window)
        where
         go = do
           dirExists <- Dir.doesDirectoryExist newDir
           if dirExists then do
             Dir.setCurrentDirectory newDir 
             currDir    <- Dir.getCurrentDirectory 
             newObjects <- getFiles currDir
             let brickList = list (window^.windowName) (V.fromList newObjects ) 1
             return $ 
               window & currentDir .~ newDir &
               objects .~ brickList & windowException .~ Nothing
           else return $ window

renderObject :: Bool -> Object -> Widget a
renderObject _ object = case object^.filetype of
     Directory -> padRight Max (str $ object^.name ++ "/")
     _         -> padRight Max (str $ object^.name)

raiseExceptionInWindow :: Window a -> IOError -> IO (Window a)
raiseExceptionInWindow window e = return $ window & windowException .~ (Just e)
renderWindow :: (Show a,Ord a) => Window a -> Widget a
renderWindow window = vBox [ renderList renderObject True (window^.objects) ]

handleWindowEvent :: (Ord a) => Vty.Event -> Window a -> EventM a (Window a)
handleWindowEvent event window =  case event of
     Vty.EvKey (Vty.KChar 'l')  []  -> handleChangeDirForward window 
     Vty.EvKey (Vty.KChar 'h')  []  -> handleChangeDirBackward window
     _                              -> do
         h <- handleListEventVi (handleListEvent) event (window^.objects) -- for characters h j k l g G
         return $ set (objects) h window

handleChangeDirForward :: (Ord a) => Window a -> EventM a (Window a)
handleChangeDirForward window = case listSelectedElement (window^.objects) of
     Just (_ , obj) -> case obj^.filetype of 
              Directory -> liftIO $ changeDir (obj^.path) window
              _         -> return window
     Nothing        -> return window 

handleChangeDirBackward :: (Ord a) => Window a -> EventM a (Window a)
handleChangeDirBackward = liftIO . changeDir ".."
 
