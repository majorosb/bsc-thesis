module Main where
import Window as W
import Browser as B
import Tabs

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified System.Directory as Dir

import Brick.AttrMap 
import Control.Lens
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Util
import Object
import System.Exit
import System.Process
import Control.Monad.IO.Class


drawUI :: Browser -> [Widget Name]
drawUI browser= pure $ withDefAttr baseAttr $
        hCenter $ B.renderBrowser browser

openFile :: Browser -> Object -> IO Browser
openFile b (Object{_name=n}) = do 
        p <- runCommand $ "xdg-open " ++ n
        waitForProcess p
        putStrLn "Press ENTER to return to the browser."
        getLine
        return b

openEditor :: Browser -> Object -> IO Browser
openEditor b (Object{_name=n}) = do 
        p <- runCommand $ (b^.defEditor) ++ " " ++ n
        waitForProcess p
        putStrLn "Press ENTER to return to the browser."
        getLine
        return b

withCheckInputMode :: (Browser -> Object -> IO Browser) -> V.Event -> Browser -> EventM Name (Next Browser)
withCheckInputMode f ev b = 
        if (b^.inputMode) || ((b^.action) /= Nothing)
          then do
            b' <- handleBrowserEvent ev b
            M.continue b'
          else case L.listSelectedElement (w^.objects) of 
            Just (_,obj) -> M.suspendAndResume $ f b obj
            Nothing      -> M.continue b
 where w = focusedWindow b


appEvent :: Browser -> BrickEvent Name e -> EventM Name (Next Browser)
appEvent b (VtyEvent ev) =
        case ev of
          V.EvKey V.KEnd []   -> M.halt b
          V.EvKey (V.KChar 'e') [] -> withCheckInputMode openEditor ev b 
          V.EvKey V.KEnter [] -> withCheckInputMode openFile ev b 
          _ -> do 
                  b' <- B.handleBrowserEvent ev b
                  M.continue b'
 where w = focusedWindow b
appEvent b _ =  M.continue b

theApp :: M.App Browser e Name
theApp = M.App {
                M.appDraw         = drawUI,
                M.appChooseCursor = M.showFirstCursor,
                M.appHandleEvent  = appEvent,
                M.appStartEvent   = return,
                M.appAttrMap      = const theMap
               }

fileColor :: V.Color
fileColor = V.rgbColor 0 95 175

dirColor :: V.Color
dirColor = V.rgbColor 215 95 0

bgColor :: V.Color
bgColor = V.rgbColor 238 238 238

tEmptyColor :: V.Color 
tEmptyColor = V.rgbColor 0 135 175

selectColor :: V.Color 
selectColor = V.rgbColor 0 135 175

selectColor2 :: V.Color
selectColor2 = V.rgbColor 215 0 135

tFocusedColor :: V.Color 
tFocusedColor = V.rgbColor 215 215 215 

fillColor :: V.Color
fillColor = V.rgbColor 0 95 135

statusLineColor :: V.Color
statusLineColor = V.rgbColor 68 68 68

theMap :: AttrMap
theMap = attrMap V.defAttr 
        [ (L.listSelectedFocusedAttr, V.white `on` selectColor ),
          (tEmpty, V.white `on` tEmptyColor),
          (tFocused, V.black `on` tFocusedColor),
          (attrFile, fg fileColor),
          (attrFill, bg fillColor),
          (attrSelected, fg selectColor2),
          (attrStatus, fg statusLineColor),
          (attrDir, fg dirColor `V.withStyle` V.bold),
          (fileTypeToAttr Directory, fg V.red),
          (L.listAttr, bg bgColor)
        ]  

main :: IO ()
main = do
        w   <- W.newWindow (WindowName 0) "."
        dir <- Dir.getCurrentDirectory
        b   <- M.defaultMain theApp =<< B.newBrowser (BrowserName "Main") (newTab (TabName dir) w)
        putStrLn "Exit success"
