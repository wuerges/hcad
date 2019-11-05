module RTree
    ( makeRTree 
    , RTree
    ) where

import Geometry

data Node = Leaf Int [Rectangle]

data RTree = Node Int Node 


makeRTree :: RTree
makeRTree = undefined