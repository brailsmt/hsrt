import System.Environment
import System.IO
import Data.List

-- Pixel datatype for recording color information for a given pixel
data Pixel = Pixel {
	red   :: Double,
	green :: Double,
	blue  :: Double
} deriving Show

-- A world coordinate
type Point = [Double]

-- The view into the world
data Viewport = Viewport {
	topLeft     :: Point,
	bottomRight :: Point
} deriving Show

-- A ray of light
data Ray = Ray {
	origin    :: Point,
	direction :: Point
} deriving Show

-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double
} deriving Show

diff :: Point -> Point -> Point
diff = zipWith (-)

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) (diff p1 p2)

normalize :: Ray -> Ray
normalize (Ray o d) = Ray o (map (/l) d)
    where 
        l = distance o d

-- dot product of two rays, unit or otherwise
dot :: Ray -> Ray -> Double
dot r1 r2 = _dot (dn r1) (dn r2)
	where 
		_dot p0 p1 = sum (zipWith (*) p0 p1)
		dn = (direction . normalize)

