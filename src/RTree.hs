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


splitList :: [(Rectangle, a)] -> ([(Rectangle, a)],[(Rectangle, a)])
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
    if length l' < maximumSize 
        then Left $ Leaf (mbr bb r) ((r,v):l)
        else Right $ ((Leaf (mbr1 $ map fst a) a), (Leaf (mbr1 $ map fst b) b))
    where 
        l' = (r,v):l
        (a, b) = splitList l'

insertNode r v (Child bb l) =
    if length l_ < maximumSize
        then Left $ Child (mbr bb mbr_) l_
        else Right (Child (mbr1 $ map fst a) a, Child (mbr1 $ map fst b) b)
    where 
        (rh, nh) : hs = sortOn (\x -> mbr r (fst x)) l
        (mbr_, l_) = case insertNode r v nh of
            Left single -> (mbr' single, (mbr' single, single):hs)
            Right (one, two) -> (mbr (mbr' one) (mbr' two), (mbr' one, one):(mbr' two, two):hs)

        (a, b) = splitList l_


-- insertNode r v (Child l) = 
--     if length l > maximumSize
--         then (Child l', Nothing)
--         else (Child a, Just $ Child [(mbr_b, Child b)])

--     where
--         (rh, nh) : hs = sortOn (\x -> mbr r (fst x)) l
--         l' = case insertNode r v nh of
--                 (a, Nothing) -> ()


--         l' = (r, v):l
--         r = case checkSplit l of
--                 Left single -> Leaf
--     Leaf $ (r, v):l
-- insertNode r v (Child l) = Child $ checkSplit hr' hn' ++ hs
--     where 
--         (h : hs) = sortOn (\x -> mbr r (fst x)) l
--         hr' :: Rectangle
--         hr' = mbr r (fst h)
--         hn' = insertNode r v (snd h)
    

-- insert r v (Node box l) = Node (minimumBoundRect r box) (r, v):
--         where n = insertLink r v