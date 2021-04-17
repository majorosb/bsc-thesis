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

data Tab = Tab {  _tabName :: Name
               ,  _tiles   :: Tiling
               ,  _renderT :: Tree Tabs.Direction Window
               ,  _ring    :: FocusRing Name
               ,  _focused :: Window
               }


makeLenses ''Tab
--             H
--       H       Leaf b
--  Leafw  Leafw'
--
--
--
--
insertTree :: (Eq b) => a -> b -> b ->  Tree a b -> Tree a b
insertTree a b t (Leaf t') = case t' == t of
                               True  -> Node a (Leaf t') (Leaf b)
                               False -> (Leaf t')
insertTree a b t (Node d t1 t2) = Node d (insertTree a b t t1) (insertTree a b t t2)

deleteTree :: (Eq b) => b ->  Tree a b -> Tree a b
deleteTree t (Leaf t') = Leaf t'
deleteTree t n@(Node d (Leaf t') (Leaf t'')) = if t' == t
                                                    then Leaf t''
                                                    else (if t'' == t then Leaf t'
                                                                      else n)
deleteTree t (Node d (Leaf t') tree) = case t' == t of
                               True  -> tree
                               False -> Node d (Leaf t')  (deleteTree t tree)
deleteTree t (Node d tree (Leaf t')) = case t' == t of
                               True  -> tree
                               False -> Node d (Leaf t') (deleteTree t tree)
deleteTree t (Node d t1 t2) = Node d (deleteTree t t1) (deleteTree t t2)

renderTree :: Tree Tabs.Direction Window -> Window  -> Widget Name
renderTree (Leaf b) f                     = if b == f then renderWindow True b else renderWindow False b
renderTree (Node Tabs.Vertical t1 t2) f   = renderTree t1 f <+> vBorder <+>  renderTree t2 f
renderTree (Node Tabs.Horizontal t1 t2) f = renderTree t1 f <=> hBorder <=>  renderTree t2 f

replaceInTree :: Window -> Tree Tabs.Direction Window -> Tree Tabs.Direction Window
replaceInTree w (Leaf w')      = if w == w' then Leaf w else Leaf w' 
replaceInTree w (Node d t1 t2) = Node d (replaceInTree w t1) (replaceInTree w t2)

getWindow :: Tree Tabs.Direction Window -> Window
getWindow (Leaf w) = w
getWindow (Node _ t t') = getWindow t


newTab :: Name -> Window -> Tab
newTab name window = Tab name [(window,Tabs.Horizontal)] (Leaf window)  focusSet window
        where
           focusL   = focusRing [window^.windowName]
           focusSet = focusSetCurrent name focusL


renderTab :: Tab -> Widget Name
renderTab t = renderTree (t^.renderT) fwindow
        where fwindow = t^.focused

-- renderTab :: Tab -> Widget Name
-- renderTab t = renderTree (t^.renderT)

--renderTiling :: Tiling -> Int ->  Widget Name
--renderTiling [(win,_)] _ = renderWindow win
--renderTiling ((w1,_):(w2,dir):xs) 1 = case dir of
--                   Tabs.Horizontal ->  renderWindow w2 <=> hBorder <=> renderTiling  xs 0
--                   Tabs.Vertical   ->  renderWindow w2 <+> vBorder <+> renderTiling  xs 0
--renderTiling ((w,dir):xs) _ = case dir of
--                   Tabs.Horizontal ->  renderWindow w <=> hBorder <=> renderTiling  xs 0
--                   Tabs.Vertical   ->  renderWindow w <+> vBorder <+> renderTiling  xs 0
--
--insertIntoTiling :: (Window,Tabs.Direction) -> Name  ->  Tiling -> Tiling
--insertIntoTiling _ _ [] = []
--insertIntoTiling w n ((win,d):xs) = 
--        if win^.windowName == n
--           then  (win,d) : w : insertIntoTiling w n xs 
--           else (win,d) : insertIntoTiling w n xs

hSplitWindow :: Tab -> Window -> Tabs.Direction -> Tab
hSplitWindow tab w direction = tab &  ring.~newFocus & renderT.~newTree
        where  
               newTree   = insertTree direction w w' (tab^.renderT)
               newRing   = focusRing $ (focusRingToList (tab^.ring)) ++ [w^.windowName]
               w'        = tab^.focused
               newFocus  = focusSetCurrent (w^.windowName) newRing
               

refreshTab :: Tab -> Window -> Tab
refreshTab t w = do t & focused.~w & renderT.~replaceInTree w (t^.renderT)
        where tiling = t^.tiles

refreshFocusedW :: Tab -> Window -> IO Tab
refreshFocusedW t w = do 
        w' <- refreshWindow w
        return $ t & focused.~w' & renderT.~replaceInTree w' (t^.renderT)
             

shiftFocus :: Tab -> Tab
shiftFocus t = t & focused.~w' & ring.~newFocus
        where w'       = goRight (t^.focused) (t^.renderT)
              newFocus = focusSetCurrent (w'^.windowName) (t^.ring)
-- Node Horizontal (Leaf l) (Leaf r) --this is bad now fix it
--
goLeft :: Window -> Tree Tabs.Direction Window -> Window 
goLeft source (Leaf _)                               = source
goLeft source (Node Tabs.Vertical (Leaf l) (Leaf r)) = if source == l then l else l
goLeft source (Node Tabs.Vertical (Leaf l) tree )    = if source == l then l else goLeft source tree
goLeft source (Node Tabs.Vertical tree _)            = goLeft source tree
goLeft source (Node Tabs.Horizontal _ _)             = source

goRight :: Window -> Tree Tabs.Direction Window -> Window 
goRight source (Leaf _) = source
goRight source (Node Tabs.Vertical (Leaf l) (Leaf r)) = if source == l then r else r
goRight source (Node Tabs.Vertical tree (Leaf r))     = if source == r then r else goRight source tree
goRight source (Node Tabs.Vertical (Leaf l) tree)     = goRight source tree
goRight source (Node Tabs.Horizontal tree tree2)      = if source == go then go else go
                                                           where go  = goRight source tree
                                                                 go2 = goRight source tree2

findWindow :: Name -> Tab -> Maybe (Window,Tabs.Direction)
findWindow name t = find (\n -> (fst n)^.windowName == name) (t^.tiles)

