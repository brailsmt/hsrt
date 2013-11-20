module HSRT where

import System.Environment
import System.IO
import Data.List
import Debug.Hood.Observe

import HSRT.Types

camera :: Ray
camera = Ray [0,0,0] [0,0,1]

-- Get all rays through the viewport for a given point.  At this time it's just a simple 1:1 ray to pixel mapping, but
-- this provides us an easy way to add subpixels and jitters
raysThroughWindow :: Point -> Point -> [Ray]
raysThroughWindow camera point = (normalize $ Ray camera [(head point), (head $ tail point), (last point)]) : []


-- For a given ray and scene, find all places where the ray intersects anything in the scene
findIntersections :: [Sphere] -> Ray -> [(Double, (Ray, Sphere))]
findIntersections objs ray = [((_intersection obj), (ray, obj)) | obj <- objs]
    where
        _intersection = (intersectionT ray)

sortIntersections :: [(Double, (Ray, Sphere))] -> [(Double, (Ray, Sphere))]
sortIntersections intersections = sortBy (\x y -> compare (fst x) (fst y)) intersections

getColorAt _ = defaultColor

-- Returns the color from shooting a ray into the scene.  This does not account for reflection.
-- In order to determine the color of a pixel in the image the colors of all reflections and refractions must be
-- averaged together.
traceRay :: [Sphere] -> Ray -> Color
traceRay scene ray 
    | intersections == [] = defaultColor
    | otherwise           = avgColor [mainColor, reflectionColor]
    where
        mainColor = getColorAt (head intersections)
        reflectionColor = traceRay scene (normalAt ray (fst $ (head intersections)) (snd $ snd (head intersections)))
        intersections = (sortIntersections . (findIntersections scene)) ray


-- This is where the magic happens.  Determine the color for each pixel in the image.
render :: Window -> [Sphere] -> Image
render win scene = Image (map (getColorAt) isects)
    where
        rays   = genRays [0,0,0] win
        isects = map (traceRay scene) rays

half :: (Double -> Double)
half = (/2.0)

-- We shoot rays through the window.  If the image is to be 8x8, then this will shoot 64 rays through the window, always
-- ensuring that a ray will shoot through each of the corners of the window and the center of the window at (0,0,1).
genRays :: Point -> Window -> [Ray]
genRays cam vp = [(Ray cam [x, y, (last $ topLeft vp)]) | x<-[fx, fx+dx..lx], y<-[fy, fy+dy..ly], x < (head wbr) && y < (head $ tail wbr)]
    where
        wtl = topLeft window
        wbr = botRight window
        dx  = (width window) / (width vp)
        dy  = (height window) / (height vp)
        fx  = head wtl + (dx/2.0)
        lx  = head wbr + (dx/2.0)
        fy  = (head $ tail wtl) + (dy/2.0)
        ly  = (head $ tail wbr) + (dy/2.0)

renderScene :: Window -> [Sphere] -> Image
renderScene win scene = render win scene

