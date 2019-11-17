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

pushMany :: [RTree a] -> MBRSet a -> MBRSet a
pushMany ts s = foldl (flip push) s ts

deque :: StateT (MBRSet a) Maybe a
deque = do
    x <- get
    (h, s') <- lift $ pop x
    case h of
        Empty -> do
            put s'
            deque
        Leaf _ v -> do
            put s'
            lift $ Just v
        Child _ ts -> do
            put $ pushMany ts s'
            deque
