module Main where
import Window as W
import Browser as B

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
import System.Exit
import Control.Monad.IO.Class


drawUI :: Browser Int -> [Widget Int]
drawUI browser= pure $ withDefAttr baseAttr $
        vBox [ hCenter $ B.renderBrowser browser]

appEvent :: Browser Int -> BrickEvent Int e -> T.EventM Int (T.Next (Browser Int))
appEvent browser (VtyEvent ev) =
        case ev of
          V.EvKey V.KEnd [] -> liftIO $ exitSuccess
          _ -> do 
                  newBrowser <- B.handleBrowserEvent ev browser 
                  M.continue newBrowser
--                  case (newWindow^.windowException) of
--                    Just e  -> handleIOException e newWindow
--                    Nothing -> M.continue newWindow
appEvent browser _ =  M.continue browser 

theApp :: M.App (Browser Int) e Int
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
        newWindow <- W.newWindow 2 "."
        b <- M.defaultMain theApp =<< B.newBrowser 1 newWindow
        putStrLn "Program exited"
        
