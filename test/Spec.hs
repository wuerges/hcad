import Geometry
import Test.QuickCheck

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
    
main :: IO ()
main = do

    putStrLn "\nTesting intersection bounding box:"
    quickCheck prop_intersectionIsInBoundingBox

    putStrLn "\nTesting intersection area"
    quickCheck prop_areaIntersectionLessthan
