module RTreeQ where

import Geometry
import RTree
import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map as Map




data MBRSet a = MBRSet Rectangle (Map Int (RTree a))


pop :: MBRSet a -> Maybe (RTree a, MBRSet a)
pop (MBRSet bb s) = do
    (h, s') <- Map.minView s
    return (h, MBRSet bb s')

push :: RTree a -> MBRSet a -> MBRSet a
push Empty x = x
push l (MBRSet bb s) = MBRSet bb (Map.insert area l s)
    where
        area = areaR $ mbr' l `minimumBoundRect` bb


deque :: StateT [RTree a] Maybe a
deque = do
    s <- get
    case s of
        [] -> lift Nothing
        (h:hs) -> case h of 
            Empty -> do
                put hs
                deque
            Leaf _ v -> do
                put hs
                lift $ Just v
            Child _ ts -> do
                put $ ts ++ hs
                deque

