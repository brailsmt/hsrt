module HSRT.Types where

import Debug.Hood.Observe

-- Constant for the max value that our PPM RGB colors can have.
ppmMaxColorValue = 255

-- Pixel datatype for recording color information for a given pixel
data Color = Color {
	red   :: Double,
	green :: Double,
	blue  :: Double
} deriving (Eq)
instance Show Color where 
  show (Color r g b) = (conv r) ++ " " ++ (conv g) ++ " " ++ (conv b) ++ "\t"
    where
      conv c = (show $ round $ c * ppmMaxColorValue)

data LightSource = LightSource {
    location :: Point,
    color    :: Color
} deriving (Show, Eq)

-- A world coordinate
type Point = [Double]

-- The window is a fixed width window through which we view the world
window = Viewport [-1,-1,1] [1,1,1]

-- The view into the world.  The top left most corner is (0, 0), the bottom right corner defines how large the image is
-- and is (width, height).
data Viewport = Viewport {
    topLeft     :: Point,
    bottomRight :: Point
} deriving (Show, Eq)
-- Build a viewport of width x heigth with the camera centered in the middle at a distance 'dist' from the viewport.  
-- The top left point is (-1/2 width, -1/2 height, dist).
-- The bottom right is (1/2 width, 1/2 height, dist).
-- The distance is the distance from the camera
mkviewport :: Double -> Double -> Double -> Viewport
mkviewport w h d = Viewport [-halfw, -halfh, d] [halfw, halfw, d]
    where
        halfw = (1/2)*w
        halfh = (1/2)*h
width :: Viewport -> Double
width (Viewport tl br) = abs $ (head tl) - (head br)

height :: Viewport -> Double
height (Viewport tl br) = abs $ (head $ tail tl) - (head $ tail br)

-- A ray of light
data Ray = Ray {
	origin    :: Point,
	direction :: Point
} deriving (Show, Eq)

class Renderable a where
    intersectionT :: Ray -> a      -> Double
    normalAt      :: Ray -> Double -> a -> Ray
    colorAt       :: Ray -> Double -> a -> Color -- This is simplistic...  the color that is contributed is dependend upon the angle to the light source

-- An image is a list of Colors, logically it is a grid of colors but using the PPM image format there is no need to do
-- anything but list the pixel values in order.  The image viewer will display the first row as Pixels [0..width] as the
-- first row, Pixels [width+1..width*2] as row 2, etc...
data Image = Image {
    viewport :: Viewport,
    imgData :: [Color]
} deriving (Show, Eq)

-- A Scene is just a list of Renderables
type Scene a = [Renderable a]

-- Returns the point in world space at some distance t from the origin
-- p = r0 + d*t
pointAt :: Ray -> Double -> Point
pointAt (Ray r0 d) t = zipWith (+) r0 (map (*t) d)

-- normalize a ray
normalize :: Ray -> Ray
normalize (Ray o d) = Ray o (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) $ diff p1 p2

-- dot product
dot a b = (sum (zipWith (*) a b))


defaultColor = Color 0 0 0

-- Subtract one point from another
diff :: Point -> Point -> Point
diff = zipWith (-) 

sumColors :: Color -> Color -> Color
sumColors c1 c2 = Color ((red c1)+(red c2)) ((green c1)+(green c2)) ((blue c1)+(blue c2))

-- Average color values over a list of colors
avgColor :: [Color] -> Color
avgColor colors = Color ((sums (red))/len) ((sums (green))/len) ((sums (blue))/len)
    where
        len = fromIntegral $ length colors
        sums f = (sum . (map (f))) colors

instance Observable Color where { observer = observeBase }
instance Observable Ray where { observer = observeBase }
instance Observable Viewport where { observer = observeBase }
instance Observable Image where { observer = observeBase }
