module HSRT.Renderable.Polygon where

import HSRT.Types
import Debug.Hood.Observe

data Polygon = Polygon {
    v1    :: Point,
    v2    :: Point,
    v3    :: Point,
    color :: Color
} deriving (Show, Eq)

instance Renderable Polygon where
    intersectionT _ _ = 2.2
    normalAt _ _ _    = Ray [0,0,0] [0,0,1]
    colorAt _ _ _     = defaultColor

instance Observable Polygon where
    observer = observeBase
