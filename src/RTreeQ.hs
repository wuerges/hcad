module RTreeQ (make, deque) where

import Geometry
import RTree
import Control.Monad.State.Lazy
import IntMultimap (IntMultimap)
import qualified IntMultimap as IM

data MBRSet a = MBRSet Rectangle (IntMultimap (RTree a))

pop :: MBRSet a -> Maybe (RTree a, MBRSet a)
pop (MBRSet bb s) = do
    (h, s') <- IM.minView s
    return (h, MBRSet bb s')

push :: RTree a -> MBRSet a -> MBRSet a
push Empty x = x
push l (MBRSet bb s) = MBRSet bb (IM.insert area l s)
    where
        area = areaR $ mbr' l `minimumBoundRect` bb

pushMany :: [RTree a] -> MBRSet a -> MBRSet a
pushMany ts s = foldl (flip push) s ts

make :: Rectangle -> RTree a -> MBRSet a
make r Empty = MBRSet r (IM.empty)
make r x     = push x $ make r Empty

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
