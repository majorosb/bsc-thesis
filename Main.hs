module Main where
import  Window as W
import qualified Graphics.Vty as V

import qualified Data.Text as Text
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import Brick.AttrMap 
import Brick.Types
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.FileBrowser as FB
import qualified Brick.AttrMap as A
import Brick.Util 
import qualified Brick.Types as T


drawUI :: Window Int -> [Widget Int]
drawUI b = [ hCenter $ W.renderWindow b ]

appEvent :: Window Int -> BrickEvent Int e -> T.EventM Int (T.Next (Window Int))
appEvent b (VtyEvent ev) =
        case ev of
          V.EvKey V.KEsc [] -> M.halt b
          _ -> do 
                  b' <- W.handleWindowEvent ev b
                  M.continue b'
appEvent b _ =  M.continue b

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
        [ (L.listSelectedFocusedAttr, V.white `on` V.yellow)]  

main :: IO ()
main = do
        b <- M.defaultMain theApp =<< W.newWindow 1
        putStrLn "asd"
