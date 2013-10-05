module HSRT.Types where

-- Pixel datatype for recording color information for a given pixel
data Color = Color {
	red   :: Double,
	green :: Double,
	blue  :: Double
} deriving (Show, Eq)

-- A world coordinate
type Point = [Double]

-- The view into the world
data Viewport = Viewport {
	topLeft     :: Point,
	bottomRight :: Point
} deriving (Show, Eq)

-- A ray of light
data Ray = Ray {
	origin    :: Point,
	direction :: Point
} deriving (Show, Eq)

class Renderable a where
    intersection :: Ray -> a      -> Double
    normalAt     :: Ray -> Double -> a -> Ray
    colorAt      :: Ray -> Double -> a -> Color

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

-- dot product of two rays
dot :: Ray -> Ray -> Double
dot (Ray _ d0) (Ray _ d1) = sum $ zipWith (*) d0 d1


defaultColor = Color 1 0 0

-- Subtract one point from another
diff :: Point -> Point -> Point
diff = zipWith (-) 

