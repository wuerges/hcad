module IntMultimap where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


newtype IntMultimap a = IntMultimap (IntMap [a])


insert :: IM.Key -> a -> IntMultimap a -> IntMultimap a
insert k v (IntMultimap m) = IntMultimap $ IM.insertWith (++) k [v] m

minView :: IntMultimap a -> Maybe (a, IntMultimap a)
minView (IntMultimap m) = do
    ((k,values), m') <- IM.minViewWithKey m
    case values of
        [] -> Nothing -- Should not happend, just to be safe
        (v:[]) -> Just $ (v, IntMultimap m')
        (v:vs)  -> Just $ (v, IntMultimap $ IM.insert k vs m')


empty :: IntMultimap a
empty = IntMultimap IM.empty


