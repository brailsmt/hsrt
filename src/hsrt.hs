module HSRT where

import System.Environment
import System.IO
import Data.List

import HSRT.Types

defaultColor = Color 1 0 0

-- Subtract one point from another
diff = zipWith (-) 

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) $ diff p1 p2

-- normalize a ray
normalize :: Ray -> Ray
normalize (Ray o d) = Ray o (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- dot product of two rays
dot :: Ray -> Ray -> Double
dot (Ray _ d0) (Ray _ d1) = sum $ zipWith (*) d0 d1

-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double,
    color  :: Color
} deriving (Show, Eq)
instance Renderable Sphere where
    intersection ray sphere = _intersection ray sphere
    normal       t   sphere = Ray [0,0,0] [0,0,1]
    colorAt      t   sphere = Color 1 0 0

mksphere :: Point -> Double -> Sphere
mksphere c r = Sphere c r defaultColor
-- Returns the point along the ray where the ray intersects the object in the scene.  Returns the point defined by 
-- p = r0 + d*t
intersectionPt :: Ray -> Double -> Point
intersectionPt (Ray o d) t = zipWith (+) o (map (*t) d)

-- Finds the closest point on the sphere where the ray intersects and returns the value for t to determine the point
-- along the ray from the formula p = r0 + d*t.
_intersection :: Ray -> Sphere -> (Double, Sphere)
_intersection ray sphere 
    | discriminant < 0 = (-1.0, sphere)
    | t0 >= 0          = (t0, sphere)
    | otherwise        = (t1, sphere)
    where 
        b  = 2 * (sum (zipWith (*) (direction ray) (diff (origin ray) (center sphere))))
        c  =     (sum $ map (^2) $ diff (origin ray) (center sphere)) - (radius sphere)^2
        discriminant = b^2 - 4*c
        t0 = (-b - sqrt discriminant)/2
        t1 = (-b + sqrt discriminant)/2


camera :: Ray
camera = Ray [0,0,0] [0,0,1]

scene :: [Sphere]
scene = [(Sphere [0,0,20] 1 (Color 0.5 0.5 1)), (Sphere [0.5,0,5] 1 (Color 0.5 1.0 0.5))]
