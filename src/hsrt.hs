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
scene = [mksphere [0,0,1] 1 (Color 0 0 1), mksphere [0,1,0] 1 (Color 0 1 0), mksphere [1,0,0] 1 (Color 1 0 0)]

-- Get all rays through the viewport for a given point.  At this time it's just a simple 1:1 ray to pixel mapping, but
-- this provides us an easy way to add subpixels and jitters
raysThroughViewportAt :: Point -> Point -> [Ray]
raysThroughViewportAt camera point = (normalize $ Ray camera [(head point), (head $ tail point), (last point)]) : []

-- For a given ray and scene, find all places where the ray intersects anything in the scene
findIntersections :: [Sphere] -> Ray -> [(Double, (Ray, Sphere))]
findIntersections objs ray = [((_intersection obj), (ray, obj)) | obj <- objs]
    where
        _intersection = intersectionT ray

sortIntersections :: [(Double, (Ray, Sphere))] -> [(Double, (Ray, Sphere))]
sortIntersections intersections = sortBy (\x y -> compare (fst x) (fst y)) intersections

getColorAt :: [(Double, (Ray, Sphere))] -> Color
getColorAt []     = defaultColor
getColorAt (t:ts) 
    | (fst t) < 0 = defaultColor
    | otherwise   = colorAt r (fst t) obj
        where
            r   = (fst$snd t)
            obj = (snd$snd t)

-- This is where the magic happens.  Determine the color for each pixel in the image.
render :: Viewport -> [Sphere] -> Image
render vport scene = Image vport (map (getColorAt) isects)
    where
        rays   = genRays [0,0,0] vport
        isects = map (sortIntersections) (map (findIntersections scene) rays)

--render' :: Viewport -> [Sphere] -> [Ray] -> [Color]
--render' vp scn rays = map (findIntersections scn) rays

genRays :: Point -> Viewport -> [Ray]
genRays cam (Viewport tl br) = [normalize (Ray cam [x, y, d]) | x<-[(head tl)..(head br)], y<-[(head $ tail tl)..(head $ tail br)]]
    where
        d = last tl

renderScene :: Viewport -> [Sphere] -> Image
renderScene vport scene = render vport scene
