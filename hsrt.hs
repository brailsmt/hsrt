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
data Coord = Coord {
	p :: [Double]
} deriving Show


-- The view into the world
data Viewport = Viewport {
	topLeft     :: Coord,
	bottomRight :: Coord
} deriving Show

-- A ray of light
data Ray = Ray {
	origin :: Coord,
	direction :: Coord
} deriving Show

-- A Sphere
data Sphere = Sphere {
	center :: Coord,
	radius :: Double
} deriving Show

-- dot product of two rays, unit or otherwise
dot :: Ray -> Ray -> Double
dot r1 r2 = _dot (dn r1) (dn r2)
	where 
		_dot (Coord x y z) (Coord i j k) = sum [(x*i), (y*j), (z*k)]
		dn = (direction . normalize)

coordDiff :: Coord -> Coord -> Coord
coordDiff (Coord x0 y0 z0) (Coord x1 y1 z1) = Coord (x0-x1) (y0-y1) (z0-z1)

normalize :: Ray -> Ray
normalize (Ray (origin) (Coord x y z)) = Ray origin (Coord (x/norm) (y/norm) (z/norm))
	where norm = (sqrt . sum . (map (^2))) [x,y,z]

distance :: Coord -> Coord -> Double
distance c1 c2 = (sqrt . sum . (map . (^2)) . (zipWith (-))) (p c1) (p c2)

length :: Ray -> Double
length (Ray origin direction) = distance o d

