module RTree
    ( makeRTree 
    , RTree
    ) where

import Geometry

data Node a = Leaf [(Rectangle, a)] | Inter [(Rectangle, Node a)]

data RTree a = Node (Node a)


makeRTree :: RTree a
makeRTree = undefined