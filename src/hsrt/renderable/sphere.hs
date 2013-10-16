module HSRT.Renderable.Sphere 
(
    Sphere,
    mksphere,
    intersection,
    normalAt,
    colorAt,
) where

import HSRT.Types
import Debug.Hood.Observe

-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double,
    color  :: Color
} deriving (Show, Eq)

mksphere :: Point -> Double -> Color -> Sphere
mksphere center radius color = Sphere center radius color

instance Observable Sphere where
    observer = observeBase

-- A sphere is renderable
instance Renderable Sphere where 
  -- Find the color at a particular point on the sphere 
  -- (this is trivial for solids, but for bit maps and bump maps it won't be)
  colorAt _ _ (Sphere _ _ c) = c

  -- Return the normal at the point where the ray collides
  normalAt ray t (Sphere c _ _) = Ray p (diff c p)
      where 
          p = pointAt ray t

  -- Finds the closest point on the sphere where the ray intersects and returns the value for t to determine the point
  -- along the ray from the formula p = r0 + d*t.
  intersection ray sphere 
      | discriminant < 0 = -1.0
      | t0 >= 0          = t0
      | otherwise        = t1
      where 
          b  = 2 * (sum (zipWith (*) (direction ray) (diff (origin ray) (center sphere))))
          c  =     (sum $ map (^2) $ diff (origin ray) (center sphere)) - (radius sphere)^2
          discriminant = b^2 - 4*c
          t0 = (-b - sqrt discriminant)/2
          t1 = (-b + sqrt discriminant)/2
