{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Browser
        where   
import qualified Graphics.Vty as Vty
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified System.Directory as Dir

import Object 
import Window
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Control.Lens
import Control.Monad.IO.Class
import System.IO.Error


data Browser a = Browser {   _window      :: Window a
                           , _browserName :: a
                           , _statusLine  :: String 
                           }

makeLenses ''Browser

statusLineW :: String -> Widget a
statusLineW s = vLimit 1 $ str s

renderBrowser :: (Ord a, Show a) => Browser a -> Widget a
renderBrowser browser = vBox [ renderWindow (browser^.window), statusLineW (browser^.statusLine)]

newBrowser :: a ->  Window a  ->  IO (Browser a)
newBrowser name window = return $ Browser window name ""


handleBrowserEvent :: (Ord a) => Vty.Event -> Browser a -> EventM a (Browser a)
handleBrowserEvent ev browser = do
        newWindow <- handleWindowEvent ev (browser^.window) 
        case (newWindow^.windowException) of
                Just e  -> return $ handleIOException e browser 
                Nothing -> return $ browser & window .~ newWindow & statusLine .~ ""

handleIOException :: IOError -> Browser a -> Browser a
handleIOException e browser = 
        case isPermissionError e of
           True  -> browser & statusLine .~ "Permission error"
           _     -> browser & statusLine .~ "error"
                               --  False -> browser
