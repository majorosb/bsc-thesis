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
import Brick.Types hiding (Direction,Horizontal,Vertical)
import Brick.Focus
import Brick.Widgets.Border.Style
import Brick.Widgets.Border 
import Data.List
import Data.Function

data Direction = Horizontal | Vertical 
data TInfo = TInfo { _dir :: Tabs.Direction
                   }
type Tiling = [(Window, Tabs.Direction)]

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
instance Functor (Tree a) where
        fmap f (Leaf w) = Leaf $ f w
        fmap f (Node d t t') = Node d (fmap f t) (fmap f t')
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
deleteTree t n@(Node d (Leaf t') (Leaf t'')) | t' == t    = Leaf t''
                                             | t'' == t   = Leaf t'   
                                             | otherwise = n          
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
             
-- Shifting focus
-- vertical :
-- Keep track of the last Vertical parent of the source
-- For example you want to go right 
-- 1st check if the source is in 

data Movement = Left | Right | Up | Down

shiftFocus :: Movement -> Tab -> Tab
shiftFocus Tabs.Right t = t & focused.~w' & ring.~newFocus
       where w'       = goRight (t^.focused) (neighborsV (t^.renderT))
             newFocus = focusSetCurrent (w'^.windowName) (t^.ring)
shiftFocus Tabs.Left t = t & focused.~w' & ring.~newFocus
       where w'       = goLeft (t^.focused) (neighborsV (t^.renderT))
             newFocus = focusSetCurrent (w'^.windowName) (t^.ring)
-- Node Horizontal (Leaf l) (Leaf r) --this is bad now fix it

                                                         
isThereV :: Tree Tabs.Direction a -> Bool
isThereV (Leaf _) = False
isThereV (Node Tabs.Vertical _ _) = True
isThereV (Node Tabs.Horizontal tree tree') = isThereV tree || isThereV tree'


getNeighbors :: Tree Direction a -> [[a]]
getNeighbors (Leaf w) = [[w]]
getNeighbors (Node Vertical (Leaf w) (Leaf w')) = [[w,w']]
getNeighbors (Node Vertical tree tree') = [goLeft' tree, goLeft' tree'] : (getNeighbors tree) ++ (getNeighbors tree')
        where
                goLeft' (Leaf w)                     = w
                goLeft' (Node _ (Leaf w) (Leaf w'))  = w
                goLeft' (Node _ (Leaf w) _)          = w
                goLeft' (Node _ tree _)              = goLeft' tree 
                goRight' (Leaf w)                    = w
                goRight' (Node _ (Leaf w) (Leaf w')) = w'
                goRight' (Node _ _ (Leaf w))         = w
                goRight' (Node _ _ tree)             = goRight' tree 
getNeighbors (Node Horizontal (Leaf w) (Leaf w')) = [[]]
getNeighbors (Node Horizontal (Leaf w) tree) = getNeighbors tree
getNeighbors (Node Horizontal tree (Leaf w)) = getNeighbors tree

makeListV :: Tree Direction a -> [[a]]
makeListV (Leaf w) = [[w]]
makeListV tt@(Node Vertical (Leaf w) (Leaf w')) = [[w] ,[w']]
makeListV tt@(Node Vertical (Leaf w) t') = makeListV t' ++ tV tt 
makeListV tt@(Node Vertical t (Leaf w)) =  makeListV t  ++ tV tt 
makeListV tt@(Node Vertical t t') =makeListV t ++ makeListV t' ++ tV tt 
makeListV (Node Horizontal (Leaf _) (Leaf _))  = [[]]
makeListV (Node Horizontal (Leaf _) t)        = makeListV t
makeListV (Node Horizontal t (Leaf _))        = makeListV t
makeListV (Node Horizontal t t')              = makeListV t ++ makeListV t'


tV :: Tree Direction a -> [[a]]
tV (Leaf w) = [[w]]
tV (Node _ t t') = [makeList t, makeList t']

makeList :: Tree Direction a -> [a]
makeList (Leaf w)            = [w]
makeList (Node _ (Leaf w) t) = w : makeList t
makeList (Node _ t (Leaf w)) = (makeList t) ++ [w]
makeList (Node _ t t')       = (makeList t) ++ (makeList t')


makeTreeName :: Tree Direction Window -> Tree Direction Name
makeTreeName t = fmap (\n -> n^.windowName) t

adjList n = filter (\n -> n /= []) $ makeListV n

adjList' []  = []
adjList' [x] = [(x,[])]
adjList' (x:y:xs) = [(x,y)] ++ adjList' xs

neighborsV :: Eq a => Tree Direction a -> [([a],[a])]
neighborsV = adjList' . filter (\n -> n /= []) .  makeListV 

goRight :: Eq a => a -> [([a],[a])] -> a
goRight source []         = source
goRight source ((f,s):xs) = if source `elem` f && s /= [] 
                               then head s 
                               else goRight source xs
              
goLeft :: Eq a => a -> [([a],[a])] -> a
goLeft source []         = source
goLeft source ((f,s):xs) = if source `elem` s && s /= [] 
                               then head f
                               else goLeft source xs

findWindow :: Name -> Tab -> Maybe (Window,Tabs.Direction)
findWindow name t = find (\n -> (fst n)^.windowName == name) (t^.tiles)
