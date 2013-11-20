-- Haskell Ray Tracer

-- A basic coordinate in 3 dimensional space
data Point = Point {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Eq)

-- make a point from a [Double]
mkpointL :: [Double] -> Point
mkpointL (pt:pts) = Point pt (head pts) (head $ tail pts)

-- make a point from a (Double, Double, Double)
mkpointT :: (Double, Double, Double) -> Point
mkpointT (x,y,z) = Point x y z

-- Decompose a point into a [Double].  This is inverse of mkpointL.  Useful for applying list operations to a Point
coords :: Point -> [Double]
coords (Point x y z) = [x, y, z]

-- Translate one point by another.  Subtract the second point from the first.
translate :: Point -> Point -> Point
translate p1 p2 = mkpointL $ zipWith (-) (coords p1) (coords p2)

-- Scale a point by some factor n.
scale :: Double -> Point -> Point
scale n point = (mkpointL . map (* n)) (coords point)

-- Find the distance between two points.  Also known as the Euclidean Distance.
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2)) . coords) $ translate p1 p2
