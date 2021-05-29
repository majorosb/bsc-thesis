module Main where
import Window as W
import Browser as B
import Tabs

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified System.Directory as Dir
import Data.List.Zipper as Z

import Brick.AttrMap 
import Control.Lens
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Util
import Brick.Themes
import Object
import System.Process
import Data.Maybe

-- | Draws the main UI.
drawUI :: Browser -> [Widget Name]
drawUI browser= pure $ withDefAttr baseAttr $
        hCenter $ B.renderBrowser browser

-- | Opens file with xdg-open.
openFile :: Browser -> Object -> IO Browser
openFile b (Object{_name=n}) = do 
        p <- runCommand $ "xdg-open " ++ n
        waitForProcess p
        putStrLn "Press ENTER to return to the browser."
        getLine
        return b
-- | Opens the user's default editor. If it doesn't exists,
-- then it tries to open it with nano.
openEditor :: Browser -> Object -> IO Browser
openEditor b (Object{_name=n}) = do 
        p <- runCommand $ (b^.defEditor) ++ " " ++ n
        waitForProcess p
        putStrLn "Press ENTER to return to the browser."
        getLine
        return b

-- | Checks the if the Browser is currently doing input or Action.
-- If not, then execute function on the currently focused Object.
-- This is useful when you dont want to mix up the events for the Action or the input.
withCheckInputMode :: (Browser -> Object -> IO Browser) 
                   -> V.Event 
                   -> Browser 
                   -> EventM Name (Next Browser)
withCheckInputMode f ev b = 
        if (b^.inputMode) || (isJust $ (b^.action)) || (isJust $ w^.help )
           -- if there is an action or input happening then just call the Browser's event handler
          then do
            b' <- handleBrowserEvent ev b
            M.continue b'
          else case L.listSelectedElement (w^.objects) of 
            Just (_,obj) -> M.suspendAndResume $ f b obj
            Nothing      -> M.continue b
 where w = focusedWindow b

-- | Main event handler. This handler decides the continuation of the event cycle.
-- For any other events except 'q' 'e' and 'Enter' calls
-- the eventHandler associated with the main Browser event handler.
appEvent :: Browser -> BrickEvent Name e -> EventM Name (Next Browser)
appEvent b (VtyEvent ev) =
        case ev of
          V.EvKey (V.KChar 'Q') [] -> M.halt b
          V.EvKey (V.KChar 'e') [] -> withCheckInputMode openEditor ev b 
          V.EvKey V.KEnter [] -> withCheckInputMode openFile ev b 
          _ -> do 
                  b' <- B.handleBrowserEvent ev b
                  M.continue b'
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
theMap = attrMap (fileColor `on` bgColor)
        [ (L.listSelectedFocusedAttr, V.white `on` selectColor ),
          (tEmpty, V.white `on` tEmptyColor),
          (tFocused, V.black `on` tFocusedColor),
          (attrFile, fg fileColor),
          (attrFill, V.white `on` fillColor),
          (attrSelected, fg selectColor2),
          (attrStatus, fg statusLineColor),
          (attrDir, fg dirColor `V.withStyle` V.bold),
          (L.listSelectedAttr, V.black `on` tFocusedColor),
          (L.listAttr, bg bgColor)
        ]  

defTheme = newTheme V.defAttr 
        [ (L.listSelectedFocusedAttr, V.white `on` selectColor ),
          (tEmpty, V.white `on` tEmptyColor),
          (tFocused, V.black `on` tFocusedColor),
          (attrFile, fg fileColor),
          (attrFill, V.white `on` fillColor),
          (attrSelected, fg selectColor2),
          (attrStatus, fg statusLineColor),
          (attrDir, fg dirColor `V.withStyle` V.bold),
          (L.listSelectedAttr, V.black `on` tFocusedColor),
          (L.listAttr, bg bgColor)
        ]

main :: IO ()
main = do
        w1   <- W.newWindow (WindowName 1) "."
        w2   <- W.newWindow (WindowName 2) "."
        w3   <- W.newWindow (WindowName 3) "."
        let tab2 = Z.insert (newTab (TabName "Tab 2") w2) empty
        let tab3 = Z.insert (newTab (TabName "Tab 3") w3) empty
        b <- B.newBrowser (BrowserName "Main") (newTab (TabName "Tab 1") w1)
        let b'  = insertIntoWs tab2 b
        let b'' = insertIntoWs tab3 b'
        _   <- M.defaultMain theApp b''
        putStrLn "Exit success"
