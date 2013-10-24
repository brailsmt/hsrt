module HSRT where

import System.Environment
import System.IO
import Data.List
import Debug.Hood.Observe

import HSRT.Types
import HSRT.Renderable

camera :: Ray
camera = Ray [0,0,0] [0,0,1]

scene :: [Sphere]
scene = [mksphere [0,0,5] 1.123 (Color 1 1 1)] --, mksphere [0.5,0,5] 1 defaultColor]

-- Get all rays through the viewport for a given point.  At this time it's just a simple 1:1 ray to pixel mapping, but
-- this provides us an easy way to add subpixels and jitters
raysThroughViewportAt :: Point -> Point -> [Ray]
raysThroughViewportAt camera point = (normalize $ Ray camera [(head point), (head $ tail point), (last point)]) : []

-- For a given ray and scene, find all places where the ray intersects anything in the scene
findIntersections :: Ray -> [Sphere] -> [(Double, Sphere)]
findIntersections ray objs = [((_intersection obj), obj) | obj <- objs]
    where
        _intersection = intersectionT ray

sortIntersections :: [(Double, Sphere)] -> [(Double, Sphere)]
sortIntersections intersections = sortBy (\x y -> compare (fst x) (fst y)) intersections

getColorAt :: Ray -> [(Double, Sphere)] -> Color
getColorAt _ []     = defaultColor
getColorAt r (t:ts) 
    | (fst t) < 0 = defaultColor
    | otherwise   = colorAt r (fst t) (snd t)

-- This is where the magic happens.  Determine the color for each pixel in the image.
render :: Viewport -> [Sphere] -> Double -> Double -> Color
render vport scene x y = getColorAt (head rays) isects
    where
        w      = width vport
        h      = height vport
        d      = last $ topLeft vport
        rays   = raysThroughViewportAt [0,0,0] [w, h, d]
        isects = sortIntersections $ findIntersections (head rays) scene

renderScene :: Viewport -> [Sphere] -> Image
renderScene vport scene = Image vport [(_render x y) | x<-[0..w],y<-[0..h]]
    where 
        w       = (width  vport) - 1
        h       = (height vport) - 1
        _render = (render vport scene)
