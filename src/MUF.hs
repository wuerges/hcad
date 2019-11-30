module MUF where

import Control.Monad.ST
import Data.Array.ST


data MUF s = 
    MUF { rank :: STUArray s Int Int
        , root :: STUArray s Int Int 
        } 


make :: Int -> ST s (MUF s)
make n = do 
    rank_ <- newArray (1,n) 1 
    root_ <- newListArray (1,n) [1..]
    return $ MUF rank_ root_


union ::MUF s ->  Int -> Int -> ST s ()
union m x y = do
    xr <- find m x
    yr <- find m y
    xrank <- readArray (rank m) xr
    yrank <- readArray (rank m) yr
    case xrank `compare` yrank of
        LT -> writeArray (root m) xr yr
        GT -> writeArray (root m) yr xr
        EQ -> do 
            writeArray (rank m) xr (xrank + yrank)
            writeArray (root m) yr xr

find :: MUF s -> Int -> ST s Int
find m x = do
    xr <- readArray (root m) x
    if xr == x 
        then return x
        else do
            xr_ <- find m xr
            writeArray (root m) x xr_
            return xr_
   
