module HSRT where

import System.Environment
import System.IO
import Data.List

import HSRT.Types


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
	radius :: Double
} deriving (Show, Eq)

r = normalize $ Ray [1,-2,-1] [1,2,4]
s = Sphere [3,0,5] 3

_intersection :: Ray -> Double -> Point
_intersection (Ray o d) t = zipWith (+) o (map (*t) d)

intersection :: Ray -> Sphere -> Point
intersection ray sphere 
    | discriminant < 0 = []
    | t0 >= 0          = _intersection ray t0
    | otherwise        = _intersection ray t1
    where 
        b  = 2 * (sum (zipWith (*) (direction ray) (diff (origin ray) (center sphere))))
        c  =     (sum $ map (^2) $ diff (origin ray) (center s)) - (radius s)^2
        discriminant = b^2 - 4*c
        t0 = (-b - sqrt discriminant)/2
        t1 = (-b + sqrt discriminant)/2
