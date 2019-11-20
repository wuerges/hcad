module MUF where

import Control.Monad.State.Lazy
import Control.Arrow
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

type MUF a b = State (Map a a, Map a Int) b

rank :: Ord a => a -> MUF a Int
rank k = fromMaybe 1 . M.lookup k . snd <$> get

make :: Ord a => a -> MUF a ()
make k = modify $ first (M.insert k k)

union :: Ord a => a -> a -> MUF a ()
union x y = do
    xr <- find x
    yr <- find y
    xrank <- rank xr
    yrank <- rank yr
    case xrank `compare` yrank of
        LT -> modify $ first (M.insert xr yr)
        GT -> modify $ first (M.insert yr xr)
        EQ -> do
            modify $ second (M.insert xr (yrank+xrank))
            modify $ first (M.insert yr xr)

find :: Ord a => a -> MUF a a
find k = fromMaybe k . M.lookup k . fst <$> get

runMUF :: MUF a b -> b
runMUF = flip evalState (M.empty, M.empty)