{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Widgets.Window
        where   
import qualified Graphics.Vty as Vty
import qualified System.Directory as Dir
import qualified System.Posix.Files as U
import qualified System.Posix.Types as U
import qualified System.FilePath as FP
import Brick.Types
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
import Brick.Widgets.List
import Widgets.Object
import Data.Set as Set
import Brick.Widgets.List

data Window n =
        Window { _currentDir :: FilePath
               , _objects :: List n Object
               , _windowName :: n
               , 
