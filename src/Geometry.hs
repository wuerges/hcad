module Geometry 
    ( Point
    , Rectangle
    , distanceP
    , distanceR
    , areaR
    , minP
    , maxP
    ) where


data Point = P [Int]
    deriving (Eq, Ord, Show)

data Rectangle = R Point Point
    deriving (Eq, Ord, Show)

coords (P cs) = cs

dsti ax1 ax2 bx1 bx2 = 
        if a < b 
            then max (b - a - aw) 0
            else max (a - b - bw) 0
    where 
        a = ax1 
        b = bx1
        aw = abs $ ax1 - ax2
        bw = abs $ bx1 - bx2

rzip (R (P p1) (P p2)) = zip p1 p2

rzipW f (R p1 p2) = pzipW f p1 p2

distanceP (P a) (P b) = zipWith (\c1 c2 -> abs (c1 - c2))
distanceR r1 r2 = sum $ map (\((ax1, ax2), (bx1, bx2)) -> dsti ax1 ax2 bx1 bx2) $ zip (rzip r1) (rzip r2)

areaR r = foldl (*) 1 $ map (\(a, b) -> abs (a - b)) $ rzip r

pzipW f (P p1) (P p2) = zipWith f p1 p2

minP = pzipW min
maxP = pzipW max

crossing (R (P (x1:y1:z1)) (P (x2:_))) (R (P (x3:y3:z3)) _) = 
    if x1 == x2 then P $ x1:y3:z3
                else P $ x3:y1:z1