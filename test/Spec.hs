import Geometry
import RTree
import RTreeQ
import Test.QuickCheck
import Control.Monad.ST
import MUF
import Data.List (sortOn, sort)

instance Arbitrary Point where
    arbitrary = do
        Positive x <- arbitrary
        Positive y <- arbitrary
        Positive z <- arbitrary
        return $ P [x, y, z]


instance Arbitrary Rectangle where
    arbitrary = do
        p1 <- arbitrary        
        p2 <- arbitrary        
        return $ R (minP p1 p2) (maxP p1 p2)
    

newtype VoidRTree = VoidRTree (RTree ())
        deriving Show

instance Arbitrary a => Arbitrary (RTree a) where
    arbitrary = do
        values <- arbitrary :: Arbitrary a => Gen [(Rectangle, a)]
        return $ foldr (uncurry insert) empty values 

instance Arbitrary VoidRTree where
    arbitrary = do
        tree <- arbitrary
        return $ VoidRTree $ tree


prop_intersectionIsInBoundingBox r1 r2 = 
    case i of 
        Just ri -> minimumBoundRect ri r2 == r2
        Nothing -> True
    where 
        i = intersection r1 r2


prop_areaIntersectionLessthan r1 r2 = 
    case mi of 
        Just i -> areaR i <= areaR r1 && areaR i <= areaR r2
        Nothing -> True
    where 
        mi = intersection r1 r2

prop_rtree_lookup_ok rs = all (flip RTree.elem t) rs
    where
        t = foldr insert' empty $ zip rs (repeat 1)
        insert' (k,v) = insert k v
        

treeHeight Empty = 0
treeHeight (Leaf _ _) = 1
treeHeight (Child _ (c:_)) = 1 + treeHeight c

prop_level_rtree_equal Empty = True
prop_level_rtree_equal (Leaf _ _) = True
prop_level_rtree_equal (Child r (c:cs)) = 
    all (treeHeight c ==) (map treeHeight cs) && all prop_level_rtree_equal (c:cs)

prop_level_voidtree_equal (VoidRTree t) = prop_level_rtree_equal t



prop_rtree_queue_ordered :: Rectangle -> [Rectangle] -> Bool
prop_rtree_queue_ordered center rects = all (\(r1, r2) -> distanceR center r1 == distanceR center r2) $ zip ordered1 ordered2
    where 
        tree = foldr (uncurry insert) empty $ zip rects rects
        ordered1 = dequeAll center tree
        ordered2 = sortOn (\r -> distanceR center r) rects


prop_rtree_queue_length :: Rectangle -> [Rectangle] -> Bool
prop_rtree_queue_length center rects = length ordered1 == length ordered2
    where 
        tree = foldr (uncurry insert) empty $ zip rects rects
        ordered1 = dequeAll center tree
        ordered2 = sortOn (\r -> distanceR center r) rects

prop_rtree_queue_elems :: Rectangle -> [Rectangle] -> Bool
prop_rtree_queue_elems center rects = ordered1 == ordered2
    where 
        tree = foldr (uncurry insert) empty $ zip rects rects
        ordered1 = sort $ dequeAll center tree
        ordered2 = sort rects
        
                        
prop_rtree_balanced :: RTree () -> Bool
prop_rtree_balanced tree = mse1 tree <= 15.0
  
prop_rtree_balanced_height :: RTree () -> Bool
prop_rtree_balanced_height tree = treeHeight tree <= 2 || (logBase 2 (fromIntegral $ count tree) >= (fromIntegral $ treeHeight tree))
  

main :: IO ()
main = do

    putStrLn "\nTesting ordering of rtree queue"
    quickCheck $ withMaxSuccess 1000 prop_rtree_queue_ordered

    putStrLn "\nTesting length of rtree queue"
    quickCheck prop_rtree_queue_length

    putStrLn "\nTesting elements of rtree queue"
    quickCheck prop_rtree_queue_elems

    putStrLn "\nTesting intersection bounding box:"
    quickCheck prop_intersectionIsInBoundingBox

    putStrLn "\nTesting intersection area"
    quickCheck prop_areaIntersectionLessthan

    putStrLn "\nTesting RTree insertion"
    quickCheck $ withMaxSuccess 1000 prop_rtree_lookup_ok

    putStrLn "\nTesting RTree balance"
    quickCheck $ withMaxSuccess 1000 prop_level_voidtree_equal

    -- putStrLn "\nTesting RTree balance mse"
    -- quickCheck $ withMaxSuccess 100000 prop_rtree_balanced

    putStrLn "\nTesting RTree balance log"
    quickCheck $ withMaxSuccess 1000 prop_rtree_balanced_height
    

    putStrLn "\nTesting MUF"
    print $ runST $ do
        muf <- MUF.make 10
        union muf 1 2
        union muf 1 3
        union muf 5 6
        union muf 3 6
        mapM (find muf) [1..10]