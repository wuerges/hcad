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
        Point p1 <- arbitrary        
        Point p2 <- arbitrary        
        return $ R (minP p1 p2) (maxP p1 p2)
    

main :: IO ()
main = putStrLn "Test suite not yet implemented"
