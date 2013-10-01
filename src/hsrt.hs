module HSRT where

import System.Environment
import System.IO
import Data.List
import HSRT.Types

-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double
} deriving (Show, Eq)

diff :: Point -> Point -> Point
diff = zipWith (-)

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) (diff p1 p2)

normalize :: Ray -> Ray
normalize (Ray o d) = Ray [0,0,0] (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- dot product of two rays
dot :: Ray -> Ray -> Double
dot (Ray _ d0) (Ray _ d1) = sum (zipWith (*) d0 d1)

--intersectSphere :: Ray -> Sphere -> Double
--intersectSphere ray sphere = 
--    where
--        l = diff (origin ray) (center sphere)
--        quada = ray `dot` ray
--        quadb = 2 * (ray `dot` (Ray [] l))


