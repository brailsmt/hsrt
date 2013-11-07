module HSRT.Renderable.Polygon where

import HSRT.Types
import Debug.Hood.Observe

instance Renderable Polygon where
    intersectionT _ _ = 2.2
    normalAt _ _ _    = Ray [0,0,0] [0,0,1]
    colorAt _ _ _     = defaultColor

instance Observable Polygon where
    observer = observeBase
