module MST where

import RTree
import MUF
import Data.Array
import Geometry
import Control.Monad.ST
import Data.Array.ST


data MST s = MST { muf :: MUF s
                 , shapes :: Array Int Rectangle
                 , obstacles :: Array Int Rectangle
                 , shape_index :: RTree Int
                 , obstacles_index :: RTree Int
                --  , STArray s Int ()
                 }


newMST :: [Rectangle] -> [Rectangle] -> ST s (MST s)
newMST ss os = do
    muf <- MUF.make (length ss) 
    let s_index = RTree.fromList $ zip ss [1..]
    let o_index = RTree.fromList $ zip os [1..]
    let s = listArray (1,length ss) ss
    let o = listArray (1,length os) os
    return $ MST muf s o s_index o_index


