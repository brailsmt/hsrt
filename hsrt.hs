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
		_dot (Coord p0) (Coord p1) = sum (zipWith (*) p0 p1)
		dn = (direction . normalize)

coordDiff :: Coord -> Coord -> Coord
coordDiff (Coord p0) (Coord p1) = Coord (zipWith (-) p0 p1)

normalize :: Ray -> Ray
normalize (Ray o d) = Ray o (Coord (map (/l) (p d)))
    where 
        l = distance o d

distance :: Coord -> Coord -> Double
distance c1 c2 = (sqrt . sum . (map (^2))) (zipWith (-) (p c1) (p c2))
