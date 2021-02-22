{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Window
        where   
import qualified Graphics.Vty as Vty
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Control.Exception as E
import qualified Data.Vector as V

import Brick.Types
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
import Brick.Widgets.List

import Object 
import Control.Lens
import Data.Set as Set
import Brick.Widgets.List



data Window n =
        Window { _currentDir :: FilePath
               , _objects :: List n Object
                          -- ^ This module provides a scrollable list type and functions for manipulating and rendering it. 
               , _windowName :: n
               , _selectedObjects :: [Object]
               , _currSelectedObject :: Object
               , _windowException :: Maybe E.IOException
               } 
makeLenses ''Window



newWindow :: a ->  IO (Window a)
newWindow name = do
        objects <- getFiles "."
        let brickList = list name (V.fromList objects) 1
        return $ Window "." brickList name [] (head objects) Nothing


renderObject :: Bool -> Object -> Widget a
renderObject bool object = padRight Max (str $ object^.name)


renderWindow :: (Show a,Ord a) => Window a -> Widget a
renderWindow window = vBox [ renderList renderObject True (window^.objects) ]

handleWindowEvent :: (Ord n) => Vty.Event -> Window n -> EventM n (Window n)
handleWindowEvent event window = do 
        h <- handleListEventVi (handleListEvent) event (window^.objects)
        return $ set (objects) h window

