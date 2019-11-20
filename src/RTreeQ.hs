-- module RTreeQ (make, deque, dequeAll) where
module RTreeQ where

import Geometry
import RTree
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
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

deque :: MaybeT (State (MBRSet a)) a
deque = do
    (h, s') <- MaybeT $ pop <$> get
    case h of
        Empty -> do
            put s'
            deque
        Leaf _ v -> do
            put s'
            MaybeT $ return $ Just v
        Child _ ts -> do
            put $ pushMany ts s'
            deque
            

dequeAllM :: State (MBRSet a) [a]
dequeAllM = do
    x <- runMaybeT deque
    case x of
        Nothing -> return []
        Just h -> (h:) <$> dequeAllM


dequeAll :: Rectangle -> RTree a -> [a]
dequeAll center tree = evalState dequeAllM $ make center tree
