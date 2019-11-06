module RTree
    ( makeRTree 
    , RTree
    ) where

import Geometry
import Data.List (sortOn)

maximumSize = 8
-- minimumSize = maximumSize `div` 2


data Node a = Leaf [(Rectangle, a)] | Child [(Rectangle, Node a)]
data BNode a = BNode Rectangle (Node a)

data RTree a = Node (Node a)


makeRTree :: Node a
makeRTree = Leaf []


splitList :: [(Rectangle, a)] -> ([(Rectangle, a)],[(Rectangle, a)])
splitList = undefined


checkSplit :: Node a -> Node a
checkSplit (Leaf l) 
    | length l < maximumSize = Leaf l
    | otherwise              = Leaf $ splitList l
checkSplit (Child l) 
    | length l < maximumSize = Child l
    | otherwise              = Child $ splitList l


insertNode :: Rectangle -> a -> Node a -> Node a
insertNode r v (Leaf l) = checkSplit $ Leaf $ (r, v):l
insertNode r v (Child l) = checkSplit $ Child $ (hr', hn'):hs
    where 
        (h : hs) = sortOn (\x -> mbr r (fst x)) l
        mbr = minimumBoundRect
        hr' :: Rectangle
        hr' = mbr r (fst h)
        hn' = insertNode r v (snd h)
    

-- insert r v (Node box l) = Node (minimumBoundRect r box) (r, v):
--         where n = insertLink r v