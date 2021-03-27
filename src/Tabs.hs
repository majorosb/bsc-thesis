{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Tabs
        where   

import qualified Graphics.Vty as Vty

import Object 
import Window
import Control.Lens
import Brick.Widgets.Core
import Brick.Types
import Brick.Focus

data Direction = Horizontal | Vertical 
data TInfo = TInfo { _dir :: Tabs.Direction
                   }
type Tiling = [(Window, Tabs.Direction)]

data Tab = Tab {  _tabName :: Name
               ,  _tiles   :: Tiling
               ,  _ring    :: FocusRing Name
               ,  _focused :: Window
               }

makeLenses ''Tab
newTab :: Name -> Window -> Tab
newTab name window = Tab name [(window,Tabs.Horizontal)] focusSet window
        where
           focusL   = focusRing [window^.windowName]
           focusSet = focusSetCurrent name focusL
 
renderTab :: Tab -> Widget Name
renderTab t = renderTiling $ t^.tiles 

renderTiling :: Tiling -> Widget Name
renderTiling [(win,_)] = renderWindow win
renderTiling ((w,dir):xs) = case dir of
                   Tabs.Horizontal -> renderTiling xs <=> renderWindow w 
                   Tabs.Vertical   -> renderTiling xs <+> renderWindow w 

-- ? if the name doesnt exists anymore in tiling it can fail
insertIntoTiling :: (Window,Tabs.Direction) -> Name  ->  Tiling -> Tiling
insertIntoTiling _ _ [] = []
insertIntoTiling w n ((win,d):xs) = 
        if win^.windowName == n
           then w : (win,d) : insertIntoTiling w n xs 
           else (win,d) : insertIntoTiling w n xs

hSplitWindow :: Tab -> Window -> Tabs.Direction -> Tab
hSplitWindow tab w direction = tab & tiles.~newTiling & ring.~newFocus
        where  newTiling = insertIntoTiling (w,direction) (tab^.focused . windowName) (tab^.tiles)
               newRing   = focusRing $ (focusRingToList (tab^.ring)) ++ [w^.windowName]
               newFocus  = focusSetCurrent (w^.windowName) newRing

refreshTab :: Tab -> (Window,Tabs.Direction) -> Tab
refreshTab t w = t & focused.~(fst w) & tiles.~(replaceInTiling tiling w)
        where tiling = t^.tiles

replaceInTiling :: Tiling -> (Window,Tabs.Direction) -> Tiling 
replaceInTiling [] w  = []
replaceInTiling ((x,d):xs) w = if xName == wName then w : replaceInTiling xs w
                                                 else (x,d) : replaceInTiling xs w
        where 
                xName = x^.windowName
                wName = (fst w)^.windowName 
