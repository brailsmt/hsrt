module HSRT where

import System.Environment
import System.IO
import Data.List

import HSRT.Types


-- Subtract one point from another
diff :: Point -> Point -> Point
diff p1 p2 = zipWith (-) p2 p1

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) (diff p1 p2)

-- normalize a ray
normalize :: Ray -> Ray
normalize (Ray o d) = Ray [0,0,0] (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- dot product of two rays
dot :: Ray -> Ray -> Double
dot (Ray _ d0) (Ray _ d1) = sum (zipWith (*) d0 d1)

mkray :: Point -> Ray
mkray p = normalize (Ray [0,0,0] p)


-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double
} deriving (Show, Eq)

intersectSphere :: Ray -> Sphere -> [Double]
intersectSphere ray sphere = [t1, t2]
    where
        v            = mkray ((origin ray) `diff` (center sphere))
        b            = 2 * (v `dot` v)
        ac           = 4 * ((v `dot` v) - ((radius sphere)^2))
        discriminant = sqrt ((b^2) - ac)
        t1           = (-b - discriminant) * 0.5
        t2           = (-b + discriminant) * 0.5

