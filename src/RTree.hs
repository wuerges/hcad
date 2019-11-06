module RTree
    ( empty
    , singleton
    , insert
    , RTree (Empty, Leaf, Child)
    , RTree.elem
    , splitList
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


splitList :: [RTree a] -> 
    Either [RTree a] (RTree a, RTree a)
splitList l = 
    if length l < maximumSize then Left l else Right $ work
    where
        (left_h, right_h):_ = sortOn (\(x,y) -> -(areaR $ mbr x y)) [(a, b) | a <- map mbr' l, b <- map mbr' l]

        (left_h_t):rem1 = sortOn (\x -> -(areaR $ mbr left_h (mbr' x))) l
        (right_h_t):rem2 = sortOn (\x -> -(areaR $ mbr left_h (mbr' x))) rem1

        -- work :: (RTree a, RTree a)
        -- work = foldl f (Child left_h [], Child right_h [])
        work = foldl f (Child left_h [left_h_t], Child right_h [right_h_t]) rem2
        
        f :: (RTree b, RTree b) -> RTree b -> (RTree b, RTree b)
        f (a@(Child ra ca), b@(Child rb cb)) t = 
            let rt = mbr' t in
            if mbr ra rt < mbr rb rt
                then (Child (mbr ra rt) (t:ca), b)
                else (a, Child (mbr rb rt) (t:cb))

mbr = minimumBoundRect

mbr1 :: [Rectangle] -> Rectangle
mbr1 l = foldl1 mbr l


mbr' :: RTree a -> Rectangle
mbr' Empty = error "empty has to mbr"
mbr' (Leaf bb _) = bb
mbr' (Child bb _) = bb

insertNodeP :: Rectangle -> a -> RTree a -> Either (RTree a) (RTree a, RTree a)

insertNodeP r v Empty = Left $ Leaf r v

insertNodeP r v (Leaf bb l) = Right (Leaf r v, Leaf bb l)

insertNodeP r v (Child bb l) =
    case splitList l_ of
        Left single -> Left $ Child (mbr bb mbr_) single
        Right (a, b) -> Right (a, b)
    where 
        h : hs = sortOn (\x -> areaR $ mbr r $ mbr' x) l
        (mbr_, l_) = case insertNodeP r v h of
            Left no_split -> (mbr (mbr' no_split) bb, no_split:hs)
            Right (one, two) -> (mbr1 [mbr' one, mbr' two, bb], one:two:hs)


insert :: Rectangle -> a -> RTree a -> RTree a
insert r v t = 
    case insertNodeP r v t of
        Left single -> single
        Right (a, b) -> Child (mbr (mbr' a) (mbr' b)) [a, b]



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
        