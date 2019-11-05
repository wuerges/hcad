module Geometry 
    ( Point
    , Rectangle
    , distanceP
    , distanceR
    , areaR
    , minP
    , maxP
    , intersection
    , crossing
    , minimumBoundRect
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


-- The 'distanceP' function calculates the manhatan distance between two points.
distanceP :: Point -> Point -> Int
distanceP (P a) (P b) = sum $ zipWith (\c1 c2 -> abs (c1 - c2)) a b

-- The 'distanceR' function calculates the manhatan distance between two rectangles.
distanceR :: Rectangle -> Rectangle -> Int
distanceR r1 r2 = sum $ map (\((ax1, ax2), (bx1, bx2)) -> dsti ax1 ax2 bx1 bx2) $ zip (rzip r1) (rzip r2)


-- The 'areaR' function calculates the area of a rectangle.
areaR :: Rectangle -> Int
areaR r = foldl (*) 1 $ map (\(a, b) -> abs (a - b)) $ rzip r

pzipW f (P p1) (P p2) = zipWith f p1 p2

-- the 'minP' function calculates the minimum point from two points.
minP :: Point -> Point -> Point
minP p1 p2 = P $ pzipW min p1 p2

-- the 'maxP' function calculates the maximum point from two points.
maxP :: Point -> Point -> Point
maxP p1 p2 = P $ pzipW max p1 p2

-- The 'crossing' function finds the point that is in the crossing of two perpendicular tracks.
crossing :: Rectangle -> Rectangle -> Point
crossing (R (P (x1:y1:z1)) (P (x2:_))) (R (P (x3:y3:z3)) _) = 
    if x1 == x2 then P $ x1:y3:z3
                else P $ x3:y1:z1

-- The 'intersection' function calculates the rectangle that is the intersection of two rectangles.
-- If the intersection is empty, the resulting rectangle will be invalid.
intersection :: Rectangle -> Rectangle -> Rectangle
intersection (R p1 p2) (R p3 p4) = 
    R (maxP p1 p3) (minP p2 p4)

-- The 'intersection' function calculates the mimum bound rectangle of two rectangles.
minimumBoundRect :: Rectangle -> Rectangle -> Rectangle
minimumBoundRect (R p1 p2) (R p3 p4) = 
    R (minP p1 p3) (maxP p2 p4)
