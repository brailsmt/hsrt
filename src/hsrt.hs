module HSRT where

import System.Environment
import System.IO
import Data.List
import Debug.Hood.Observe

import HSRT.Types
import HSRT.Renderable.Sphere

camera :: Ray
camera = Ray [0,0,0] [0,0,1]

scene :: [Sphere]
scene = [Sphere [0,0,1] 1 (Color 0 0 1), Sphere [0,1,0] 1 (Color 0 1 0), Sphere [1,0,0] 1 (Color 1 0 0)]

-- Get all rays through the viewport for a given point.  At this time it's just a simple 1:1 ray to pixel mapping, but
-- this provides us an easy way to add subpixels and jitters
raysThroughViewportAt :: Point -> Point -> [Ray]
raysThroughViewportAt camera point = (normalize $ Ray camera [(head point), (head $ tail point), (last point)]) : []


-- For a given ray and scene, find all places where the ray intersects anything in the scene
findIntersections :: [Sphere] -> Ray -> [(Double, (Ray, Sphere))]
findIntersections objs ray = [((_intersection obj), (ray, obj)) | obj <- objs]
    where
        _intersection = (intersectionT ray)

sortIntersections :: [(Double, (Ray, Sphere))] -> [(Double, (Ray, Sphere))]
sortIntersections intersections = sortBy (\x y -> compare (fst x) (fst y)) intersections

--getColorAt :: Intersection -> Color
--getColorAt (t:ts) 
--    | (fst t) < 0 = defaultColor
--    | otherwise   = colorAt r (fst t) obj
--        where
--            r   = (fst$snd t)
--            obj = (snd$snd t)
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
render :: Viewport -> [Sphere] -> Image
render vport scene = Image vport (map (getColorAt) isects)
    where
        rays   = genRays [0,0,0] vport
        isects = map (traceRay scene) rays

--render' :: Viewport -> [Sphere] -> [Ray] -> [Color]
--render' vp scn rays = map (findIntersections scn) rays

-- We shoot rays through the window.  If the image is to be 8x8, then this will shoot 64 rays through the window, always
-- ensuring that a ray will shoot through each of the corners of the window and the center of the window at (0,0,1).
genRays :: Point -> Viewport -> [Ray]
genRays cam vp = [(Ray cam [x, y, (last $ topLeft vp)]) | x<-[fx, fx+dx..lx], y<-[fy, fy+dy..ly], x < (head wbr) && y < (head $ tail wbr)]
    where
        wtl = topLeft window
        wbr = bottomRight window
        dx  = (width window) / (width vp)
        dy  = (height window) / (height vp)
        fx  = head wtl + (dx/2.0)
        lx  = head wbr + (dx/2.0)
        fy  = (head $ tail wtl) + (dy/2.0)
        ly  = (head $ tail wbr) + (dy/2.0)

renderScene :: Viewport -> [Sphere] -> Image
renderScene vport scene = render vport scene

