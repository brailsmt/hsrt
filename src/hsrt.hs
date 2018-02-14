module HSRT where

import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.IfElse

{- BEGIN Point related functions -}
-- A basic coordinate in 3 dimensional space
data Point = Point {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Eq)

mkpoint :: Double ->Double ->Double ->Point
mkpoint x y z = Point x y z

-- make a point from a list of [Double], only the first 3 values in the array are considered.
mkpointL :: [Double] ->Maybe Point
mkpointL (pt:pts)
  | length pts == 2 = Just $ Point x y z
  | otherwise      = Nothing
    where
        x = pt
        y = head pts
        z = (head . tail) pts

-- Decompose a point into a [Double].  Useful for applying list operations to a Point.
coords :: Point ->[Double]
coords (Point x y z) = [x, y, z]

-- Translate one point by another.  Subtract the second point from the first.
translate :: Point ->Point ->Point
translate (Point x1 y1 z1) (Point x2 y2 z2) = mkpoint (x1 - x2) (y1 - y2) (z1 - z2)

-- Scale a point by some factor n.
scale :: Double ->Point ->Point
scale n (Point x y z) = mkpoint (x * n) (y * n) (z * n)

-- Find the distance between two points.  Also known as the Euclidean Distance.
distance :: Point ->Point ->Double
distance p1 p2 = (sqrt . sum . (map (^2)) . coords) $ translate p1 p2

-- Determine the dot product between two points
dot a b = (sum (zipWith (*) (coords a) (coords b)))
{- END Point related functions -}


{- BEGIN Color related functions -}
data Color = Color {
      red   :: Double,
      green :: Double,
      blue  :: Double
    --, alpha :: Double
} deriving Eq
instance Show Color where
    show (Color r g b) = (conv r) ++ " " ++ (conv g) ++ " " ++ (conv b) ++ "\t"
        where
            conv c = (show $ round  $ c * 255)

type Colors = [Color]

addColors :: Color ->Color ->Color
addColors (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
{- END Color related functions -}

{- BEGIN Ray related definitions/functions -}
data Ray = Ray {
    origin    :: Point,
    direction :: Point
} deriving Show
{- END Ray related definitions/functions -}


{- BEGIN Sphere related definitions/functions -}
data Sphere = Sphere {
    center :: Point,
    radius :: Double,
    color  :: Color
} deriving (Show, Eq)


-- Finds the closest point on the sphere where the ray 
-- intersects and returns the value for t to determine 
-- the point along the ray from the formula p = r0 + d*t.
intersection :: Ray ->Sphere ->Maybe Double
intersection ray sphere 
    | discriminant < 0 = Nothing
    | t0 >= 0           = Just t0
    | otherwise        = Just t1
    where 
        v  = translate (origin ray) (center sphere)
        d  = direction ray
        a  = d `dot` d
        b  = v `dot` d
        c  = v `dot` v - (radius sphere)^2
        t0  = ((-b) - (sqrt discriminant))/ a
        t1  = ((-b) + (sqrt discriminant))/ a
        discriminant = (b^2) - (a * c)
{- END Sphere related definitions/functions -}


-- The viewport is a fixed width window centered at (0,0,0) with width an height of 2 (to simplify some math) 
-- through which we shoot rays.  The Point tuple defines the top left and bottom right corner
viewport = ((Point (-1) 1 0), (Point 1 (-1) 0))
viewportWidth  = 2.0
viewportHeight = 2.0

-- A Scene is a list of objects (Spheres only) and lights (Points only).
data Scene = Scene {
    objects  :: [Sphere],
    lights   :: [Point]
} deriving Show

-- width x height
imageDimensions = (32, 32)

-- This is an array of colors forming the image.  It has the length of width x height of the image dimensions.
-- It can be thought of as an array of arrays like so:
-- [ 
--   [Scanline1],
--   [Scanline2],
--   ...
--   [ScanlineN]
-- ]
-- Image[(width * N)..(width * N) + width] == scanline N
-- Image[0] = The Color of the first pixel in Scanline1
type Image = [Color]

-- Generate width x height rays from the camera through the viewport.  This simply returns a list of Rays for each pixel
-- in the image from the camera through the center of each pixel.  

--   1) divide the image width by the viewport width, call it dx
--   2) divide the image height by the viewport height, call it dy
--   3) beginning at y = -1 + dy/2
--   4) for each y until y = 1 - dy/2
--   5)   beginning at x = -1 + dx/2
--   6)   for each x until x = 1 - dx/2
--   5)     generate a normalized ray with origin at the camera and passing through (x, y, 1)
rayGenerator :: Point ->(Point, Point) ->(Integer, Integer) ->Double ->[Ray]
rayGenerator camera viewport dimensions viewportDistance = [(Ray camera (Point x y 1))| y <- [y0, y0+dy..yf], x <- [x0, x0+dx..xf]]
    where
        y0 = (-1) + (dy/2)
        yf = 1 - (dy/2)
        x0 = (-1) + (dx/2)
        xf = 1 - (dx/2)
        dx = viewportWidth/(fromIntegral $ fst dimensions)
        dy = viewportHeight/(fromIntegral $ snd dimensions)


colorAt :: Sphere ->Ray ->Color
colorAt sphere ray = 
    case (intersection ray sphere) of 
      Nothing   ->Color 0 0 0
      Just d ->color sphere

traceScene :: Scene ->(Integer, Integer) ->Double ->[Color]
traceScene scene dimensions viewportDistance = [colorAt sphere ray | ray <- (rayGenerator (Point 0 0 0) viewport dimensions viewportDistance), sphere <- (objects scene)]

plitUp [] _ = []
splitUp list width = take width list : splitUp (drop width list) width

