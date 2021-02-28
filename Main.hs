module Main where
import Window as W

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Brick.Types as T

import Brick.AttrMap 
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Util
import Object


drawUI :: Window Int -> [Widget Int]
drawUI window = pure $ withDefAttr baseAttr $ vBox [ hCenter $ W.renderWindow window]

statusLine ::  String ->  Widget Int
statusLine s = vLimit 1 $ str s


appEvent :: Window Int -> BrickEvent Int e -> T.EventM Int (T.Next (Window Int))
appEvent window (VtyEvent ev) =
        case ev of
          V.EvKey V.KEsc [] -> M.halt window
          _ -> do 
                  newWindow <- W.handleWindowEvent ev window
                  M.continue newWindow
--                  case (newWindow^.windowException) of
--                    Just e  -> handleIOException e newWindow
--                    Nothing -> M.continue newWindow
appEvent window _ =  M.continue window

theApp :: M.App (Window Int) e Int
theApp = M.App {
                M.appDraw = drawUI,
                M.appChooseCursor = M.showFirstCursor,
                M.appHandleEvent = appEvent,
                M.appStartEvent = return,
                M.appAttrMap    = const theMap
               }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr 
        [ (L.listSelectedFocusedAttr, V.white `on` V.yellow),
          (fileTypeToAttr File, V.white `on` V.red),
          (fileTypeToAttr Directory, fg V.red)
        ]  

main :: IO ()
main = do
        b <- M.defaultMain theApp =<< W.newWindow 1 "."
        putStrLn "Program exited"
        
