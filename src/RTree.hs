module RTree
    ( empty
    , singleton
    , insert
    , RTree (Empty, Leaf, Child)
    , RTree.elem
    ) where

import Geometry
import Data.List (sortOn)
import Control.Arrow

maximumSize = 8
-- minimumSize = maximumSize `div` 2


data RTree a = Empty | Leaf Rectangle a | Child Rectangle [RTree a]
        deriving Show


empty :: RTree a
empty = Empty

singleton :: Rectangle -> a -> RTree a
singleton = Leaf


-- builds all pairs from combining every rectangle with another from the list
mbrs2 :: [Rectangle] -> Rectangle -> [Rectangle] -> [(Rectangle, Rectangle)]
mbrs2 pre r [] = zip (repeat r) pre
mbrs2 pre r (p:ost) = 
    zip (repeat r) pre ++
    zip (repeat r) (p:ost) ++ 
    mbrs2 (r:pre) p ost

-- iterates trough a list of rtrees and splits them according to the smallest mbr
select :: [RTree a] -> [RTree a] -> Rectangle -> Rectangle -> [RTree a] -> ([RTree a], [RTree a])
select l1 l2 _ _ [] = (l1, l2)
select l1 l2 r1 r2 (l:ls) = 
    if mbr' l `mbr` r1 < mbr' l `mbr` r2 
        then select (l:l1) l2 r1 r2 ls
        else select l1 (l:l2) r1 r2 ls

-- splits a list of rtrees into two rtrees according to their mbrs
splitNode :: [RTree a] -> (RTree a, RTree a)
splitNode l = (Child (mbr1 rt1') rt1', Child (mbr1 rt2') rt2')
    where
        (r1, r2):_ = sortOn leastArea (mbrs2 [] r ects)
        (r:ects) = map mbr' l
        leastArea :: (Rectangle, Rectangle) -> Int
        leastArea (a, b) = -(areaR $ mbr a b)
        (rt1, rt2) = select [] [] r1 r2 l
        (rt1', rt2') = 
            if length rt1 == 0 then ([head rt2], tail rt2)
            else if length rt2 == 0 then (tail rt1, [head rt1])
                 else (rt1, rt2)
        
mbr = minimumBoundRect

mbr1 :: [RTree a] -> Rectangle
mbr1 l = foldl1 mbr $ map mbr' l

mbr' :: RTree a -> Rectangle
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


elem :: Rectangle -> RTree a -> Bool
elem _ Empty = False
elem r (Leaf bb _) = 
    case intersection r bb of
        Nothing -> False
        Just _ -> True 
        
elem r (Child bb l) = 
    case intersection r bb of
        Nothing -> False
        Just _ -> any (RTree.elem r) l
        