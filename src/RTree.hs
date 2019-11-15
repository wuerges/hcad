module RTree
    ( empty
    , singleton
    , insert
    , RTree
    ) where

import Geometry
import Data.List (sortOn)
import Control.Arrow

maximumSize = 8
-- minimumSize = maximumSize `div` 2


data RTree a = Empty | Leaf Rectangle a | Child Rectangle [RTree a]


empty :: RTree a
empty = Empty

singleton :: Rectangle -> a -> RTree a
singleton = Leaf




splitNode :: [RTree a] -> (RTree a, RTree a)
splitNode l = undefined
    -- if length l < maximumSize

-- splitList :: [(Rectangle, a)] -> 
--     Either [(Rectangle, a)] (Rectangle, [(Rectangle, a)], Rectangle, [(Rectangle, a)])
-- splitList l = 
--     if length l < maximumSize then Left l else Right $ work l
--     where
--         (left_h, right_h):rem = sortOn (\(x,y) -> -(areaR $ mbr x y)) [(a, b) | a <- map fst l, b <- map fst l]

--         work = foldl f (left_h, [], right_h, [])
--         f (rect_a, a, rect_b, b) (r, v) = 
--             if mbr r rect_a < mbr r rect_b 
--                 then (mbr r rect_a, (r,v):a,       rect_b,       b)
--                 else (      rect_a,       a, mbr r rect_b, (r,v):b)

mbr = minimumBoundRect

mbr1 :: [RTree a] -> Rectangle
mbr1 l = foldl1 mbr $ map mbr' l


mbr' Empty = error "empty has to mbr"
mbr' (Leaf bb _) = bb
mbr' (Child bb _) = bb

insertNodeP :: Rectangle -> a -> RTree a -> Either (RTree a) (RTree a, RTree a)

insertNodeP r v Empty = Left $ Leaf r v

insertNodeP r v (Leaf bb l) = Right (Leaf r v, Leaf bb l)

insertNodeP r v (Child bb l) =
    case insertNodeP r v h of
        -- if there was no split, just update the mbr of the current node
        Left no_split -> Left $ Child (mbr' no_split `mbr` bb) (no_split:hs)
        -- if there was a split, checks if the current node will also have a split
        Right (one, two) -> 
            if length hs + 2 < maximumSize
                -- if there will not be a split, just update the current node mbr 
                -- and insert the new head to the tree
                then Left $ Child (mbr' one `mbr` mbr' two `mbr` bb) (one:two:hs)
                -- if there will be overflow, split the node
                else Right $ splitNode (one:two:hs)

    where 
        -- selects the child that will have the smallest rectangle
        h : hs = sortOn (\x -> r `mbr` mbr' x) l 


insert :: Rectangle -> a -> RTree a -> RTree a
insert r v t = 
    case insertNodeP r v t of
        Left single -> single
        Right (a, b) -> Child (mbr' a `mbr` mbr' b) [a, b]

