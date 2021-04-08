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
import Brick.Widgets.Border.Style
import Brick.Widgets.Border 
import Data.List



data Direction = Horizontal | Vertical 
data TInfo = TInfo { _dir :: Tabs.Direction
                   }
type Tiling = [(Window, Tabs.Direction)]

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
                 --  ^ Window ^Tabs.Direction
                 --  Node Vertical (Leaf Window1) (Leaf Window2)

insertTree :: (Eq b) => a -> b -> b ->  Tree a b -> Tree a b
insertTree a b t (Leaf t') = case t' == t of
                               True  -> Node a (Leaf t') (Leaf b)
                               False -> (Leaf t')
insertTree a b t (Node d t1 t2) = Node d (insertTree a b t t1) (insertTree a b t t2)

renderTree :: Tree Tabs.Direction Window -> Widget Name
renderTree (Leaf b)                     = renderWindow b
renderTree (Node Tabs.Horizontal t1 t2) = renderTree t1 <+> vBorder <+>  renderTree t2
renderTree (Node Tabs.Vertical t1 t2)   = renderTree t1 <=> hBorder <=>  renderTree t2

replaceInTree :: Window -> Tree Tabs.Direction Window -> Tree Tabs.Direction Window
replaceInTree w (Leaf w')      = if w == w' then Leaf w else Leaf w' 
replaceInTree w (Node d t1 t2) = Node d (replaceInTree w t1) (replaceInTree w t2)

data Tab = Tab {  _tabName :: Name
               ,  _tiles   :: Tiling
               ,  _renderT :: Tree Tabs.Direction Window
               ,  _ring    :: FocusRing Name
               ,  _focused :: Window
               }


makeLenses ''Tab

newTab :: Name -> Window -> Tab
newTab name window = Tab name [(window,Tabs.Horizontal)] (Leaf window)  focusSet window
        where
           focusL   = focusRing [window^.windowName]
           focusSet = focusSetCurrent name focusL


renderTab' :: Tab -> Widget Name
renderTab' t = renderTree (t^.renderT)

renderTab :: Tab -> Widget Name
renderTab t = renderTree (t^.renderT)

renderTiling :: Tiling -> Int ->  Widget Name
renderTiling [(win,_)] _ = renderWindow win
renderTiling ((w1,_):(w2,dir):xs) 1 = case dir of
                   Tabs.Horizontal ->  renderWindow w2 <=> hBorder <=> renderTiling  xs 0
                   Tabs.Vertical   ->  renderWindow w2 <+> vBorder <+> renderTiling  xs 0
renderTiling ((w,dir):xs) _ = case dir of
                   Tabs.Horizontal ->  renderWindow w <=> hBorder <=> renderTiling  xs 0
                   Tabs.Vertical   ->  renderWindow w <+> vBorder <+> renderTiling  xs 0

insertIntoTiling :: (Window,Tabs.Direction) -> Name  ->  Tiling -> Tiling
insertIntoTiling _ _ [] = []
insertIntoTiling w n ((win,d):xs) = 
        if win^.windowName == n
           then  (win,d) : w : insertIntoTiling w n xs 
           else (win,d) : insertIntoTiling w n xs

hSplitWindow :: Tab -> Window -> Tabs.Direction -> Tab
hSplitWindow tab w direction = tab & tiles.~newTiling & ring.~newFocus & renderT.~newTree
        where  newTiling = insertIntoTiling (w,direction) (tab^.focused . windowName) (tab^.tiles)
               newTree   = insertTree direction w w' (tab^.renderT)
               newRing   = focusRing $ (focusRingToList (tab^.ring)) ++ [w^.windowName]
               w'        = tab^.focused
               newFocus  = focusSetCurrent (w^.windowName) newRing
               

refreshTab :: Tab -> (Window,Tabs.Direction) -> Tab
refreshTab t w = t & focused.~(fst w) & tiles.~(replaceInTiling tiling w) & renderT.~(replaceInTree (fst w) (t^.renderT))
        where tiling = t^.tiles

replaceInTiling :: Tiling -> (Window,Tabs.Direction) -> Tiling 
replaceInTiling [] w  = []
replaceInTiling ((x,d):xs) w = if xName == wName then w : replaceInTiling xs w
                                                 else (x,d) : replaceInTiling xs w
        where 
                xName = x^.windowName
                wName = (fst w)^.windowName 

shiftFocus :: Tab -> Tab
shiftFocus t = case newW of 
                 Just a  -> t & focused.~ a & ring.~newFocus
                 Nothing -> t
        where newFocus = focusNext $ t^.ring
              newW     = do
                      curr  <- focusGetCurrent newFocus
                      (w,_) <- find (\n -> (fst n)^.windowName == curr) (t^.tiles)
                      return w 

findWindow :: Name -> Tab -> Maybe (Window,Tabs.Direction)
findWindow name t = find (\n -> (fst n)^.windowName == name) (t^.tiles)

