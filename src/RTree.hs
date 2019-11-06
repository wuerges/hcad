module RTree
    ( empty
    , singleton
    , insert
    , RTree
    , RTree.elem
    ) where

import Geometry
import Data.List (sortOn)
import Control.Arrow

maximumSize = 8
-- minimumSize = maximumSize `div` 2


data RTree a = Empty | Leaf Rectangle a | Child Rectangle [(Rectangle, RTree a)]


empty :: RTree a
empty = Empty

singleton :: Rectangle -> a -> RTree a
singleton = Leaf


splitList :: [(Rectangle, a)] -> 
    Either [(Rectangle, a)] (Rectangle, [(Rectangle, a)], Rectangle, [(Rectangle, a)])
splitList l = 
    if length l < maximumSize then Left l else Right $ work l
    where
        (left_h, right_h):rem = sortOn (\(x,y) -> -(areaR $ mbr x y)) [(a, b) | a <- map fst l, b <- map fst l]

        work :: [(Rectangle, b)] -> (Rectangle, [(Rectangle, b)], Rectangle, [(Rectangle, b)])
        work = foldl f (left_h, [], right_h, [])
        
        f :: (Rectangle, [(Rectangle, b)], Rectangle, [(Rectangle, b)]) -> (Rectangle, b) -> (Rectangle, [(Rectangle, b)], Rectangle, [(Rectangle, b)])
        f (rect_a, a, rect_b, b) (r, v) = 
            if mbr r rect_a < mbr r rect_b 
                then (mbr r rect_a, (r,v):a,       rect_b,       b)
                else (      rect_a,       a, mbr r rect_b, (r,v):b)

mbr = minimumBoundRect

mbr1 :: [Rectangle] -> Rectangle
mbr1 l = foldl1 mbr l


mbr' Empty = error "empty has to mbr"
mbr' (Leaf bb _) = bb
mbr' (Child bb _) = bb

insertNodeP :: Rectangle -> a -> RTree a -> Either (RTree a) (RTree a, RTree a)

insertNodeP r v Empty = Left $ Leaf r v

insertNodeP r v (Leaf bb l) = Right (Leaf r v, Leaf bb l)

insertNodeP r v (Child bb l) =
    case splitList l_ of
        Left single -> Left $ Child (mbr bb mbr_) single
        Right (mbr_a, a, mbr_b, b) -> Right (Child mbr_a a, Child mbr_b b)
    where 
        (rh, nh) : hs = sortOn (\x -> mbr r (fst x)) l
        (mbr_, l_) = case insertNodeP r v nh of
            Left no_split -> (mbr' no_split, (mbr' no_split, no_split):hs)
            Right (one, two) -> (mbr (mbr' one) (mbr' two), (mbr' one, one):(mbr' two, two):hs)


insert :: Rectangle -> a -> RTree a -> RTree a
insert r v t = 
    case insertNodeP r v t of
        Left single -> single
        Right (a, b) -> Child (mbr mbr_a mbr_b) [(mbr_a, a), (mbr_b, b)]
            where 
                mbr_a = mbr' a
                mbr_b = mbr' b



elem :: Rectangle -> RTree a -> Bool
elem _ Empty = False
elem r (Leaf bb _) = 
    case intersection r bb of
        Nothing -> False
        Just _ -> True 

elem r (Child bb l) = 
    case intersection r bb of
        Nothing -> False
        Just _ -> any (RTree.elem r) (map snd l)
        