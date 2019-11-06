module RTree
    ( makeRTree 
    , RTree
    ) where

import Geometry
import Data.List (sortOn)
import Control.Arrow

maximumSize = 8
-- minimumSize = maximumSize `div` 2


data Node a = Empty | Leaf Rectangle [(Rectangle, a)] | Child Rectangle [(Rectangle, Node a)]
data BNode a = BNode Rectangle (Node a)

data RTree a = Node (Node a)


makeRTree :: Node a
makeRTree = Empty


-- splitList :: [(Rectangle, a)] -> ([(Rectangle, a)],[(Rectangle, a)])
splitList :: [(Rectangle, a)] -> 
    Either [(Rectangle, a)] (Rectangle, [(Rectangle, a)], Rectangle, [(Rectangle, a)])
splitList = undefined

mbr = minimumBoundRect

-- checkSplit :: [(Rectangle, a)] -> 
-- checkSplit :: Rectangle -> Node a -> [(Rectangle, Node a)]
-- checkSplit = undefined
-- checkSplit l
--     | length l < maximumSize = l
--     | otherwise              = Child $ splitList l



mbr1 :: [Rectangle] -> Rectangle
mbr1 l = foldl1 mbr l


mbr' Empty = error "empty has to mbr"
mbr' (Leaf bb _) = bb
mbr' (Child bb _) = bb

insertNode :: Rectangle -> a -> Node a -> Either (Node a) (Node a, Node a)

insertNode r v Empty = Left $ Leaf r [(r,v)]

insertNode r v (Leaf bb l) = 
    case splitList l' of
        Left single -> Left $ Leaf (mbr bb r) single
        Right (mbr_a, a, mbr_b, b) -> Right (Leaf mbr_a a, Leaf mbr_b b)
    where 
        l' = (r,v):l


insertNode r v (Child bb l) =
    case splitList l_ of
        Left single -> Left $ Child (mbr bb mbr_) single
        Right (mbr_a, a, mbr_b, b) -> Right (Child mbr_a a, Child mbr_b b)
    where 
        (rh, nh) : hs = sortOn (\x -> mbr r (fst x)) l
        (mbr_, l_) = case insertNode r v nh of
            Left no_split -> (mbr' no_split, (mbr' no_split, no_split):hs)
            Right (one, two) -> (mbr (mbr' one) (mbr' two), (mbr' one, one):(mbr' two, two):hs)


