module HSRT.Renderable.Sphere where

import HSRT.Types
import Debug.Hood.Observe

instance Observable Sphere where
    observer = observeBase

-- A sphere is renderable...  
instance Renderable Sphere where 
  -- Find the color at a particular point on the sphere 
  -- (this is trivial for solids, but for bit maps and bump maps it won't be)
  colorAt _ _ (Sphere _ _ c) = c

  -- Return the normal at the point where the ray collides
  normalAt ray t (Sphere c _ _) = Ray p (diff c p)
      where 
          p = pointAt ray t

  -- Finds the closest point on the sphere where the ray 
  -- intersects and returns the value for t to determine 
  -- the point along the ray from the formula p = r0 + d*t.
  intersectionT ray sphere 
      | discriminant < 0 = -1.0
      | t0 >= 0          = t0
      | otherwise        = t1
      where 
          v  = diff (origin ray) (center sphere)
          d  = direction ray
          a  = d `dot` d
          b  = v `dot` d
          c  = v `dot` v - (radius sphere)^2
          t0  = ((-b) - (sqrt discriminant))/ a
          t1  = ((-b) + (sqrt discriminant))/ a
          discriminant = (b^2) - (a * c)
