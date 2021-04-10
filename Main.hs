module Main where
import Window as W
import Browser as B
import Tabs

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Brick.Types 
import qualified System.Directory as Dir

import Brick.AttrMap 
import Brick.Types
import Control.Lens
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Util
import Object
import System.Exit
import Control.Monad.IO.Class


drawUI :: Browser -> [Widget Name]
drawUI browser= pure $ withDefAttr baseAttr $
        hCenter $ B.renderBrowser browser

appEvent :: Browser -> BrickEvent Name e -> EventM Name (Next Browser)
appEvent browser (VtyEvent ev)    =
        case ev of
          V.EvKey V.KEnd [] -> liftIO $ exitSuccess
          _ -> do 
                  newBrowser <- B.handleBrowserEvent ev browser 
                  M.continue newBrowser
--                  case (newWindow^.windowException) of
--                    Just e  -> handleIOException e newWindow
--                    Nothing -> M.continue newWindow
appEvent browser _ =  M.continue browser 

theApp :: M.App Browser e Name
theApp = M.App {
                M.appDraw = drawUI,
                M.appChooseCursor = M.showFirstCursor,
                M.appHandleEvent = appEvent,
                M.appStartEvent = return,
                M.appAttrMap    = const theMap
               }
fileColor :: V.Color
fileColor = V.rgbColor 0 95 135

dirColor :: V.Color
dirColor = V.rgbColor 215 95 0

bgColor :: V.Color
bgColor = V.rgbColor 238 238 238

tEmptyColor :: V.Color 
tEmptyColor = V.rgbColor 0 135 175

selectColor :: V.Color 
selectColor = V.rgbColor 0 135 175

tFocusedColor :: V.Color 
tFocusedColor = V.rgbColor 188 188 188

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr 
        [ (L.listSelectedFocusedAttr, V.white `on` selectColor ),
          (tEmpty, V.white `on` tEmptyColor),
          (tFocused, V.white `on` tFocusedColor),
          (attrFile, fg fileColor),
          (attrDir, fg dirColor `V.withStyle` V.bold),
          (fileTypeToAttr Directory, fg V.red),
          (L.listAttr, bg bgColor)
        ]  

main :: IO ()
main = do
        newWindow <- W.newWindow (WindowName 0) "."
        dir       <- Dir.getCurrentDirectory
        b <- M.defaultMain theApp =<< B.newBrowser (BrowserName "Main") (newTab (TabName dir) newWindow)
        putStrLn "Program exited"
        
