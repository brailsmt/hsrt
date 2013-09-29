import System.Environment
import System.IO
import Data.List
import Test.QuickCheck

-- Pixel datatype for recording color information for a given pixel
data Pixel = Pixel {
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

-- A Sphere
data Sphere = Sphere {
	center :: Point,
	radius :: Double
} deriving (Show, Eq)

diff :: Point -> Point -> Point
diff = zipWith (-)

-- Euclidian distance between two points in n-space
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) (diff p1 p2)

normalize :: Ray -> Ray
normalize (Ray o d) = Ray [0,0,0] (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- dot product of two rays
dot :: Ray -> Ray -> Double
dot (Ray _ d0) (Ray _ d1) = sum (zipWith (*) d0 d1)

--intersectSphere :: Ray -> Sphere -> Double
--intersectSphere ray sphere = 
--    where
--        l = diff (origin ray) (center sphere)
--        quada = ray `dot` ray
--        quadb = 2 * (ray `dot` (Ray [] l))

---------------------------------------------------
-- QuickCheck testing
---------------------------------------------------

-- Generate random Rays
instance Arbitrary Ray where
    arbitrary = do
        p1 <- vectorOf 3 (arbitrary::Gen Double)
        p2 <- vectorOf 3 (arbitrary::Gen Double)
        return $ Ray p1 p2

-- The idempotency fails due to floating point precision issues...
--prop_normalIdempotency ray = (direction ray) /= [0,0,0] ==> (normalize . normalize) ray == (normalize . normalize . normalize) ray
prop_lengthOfNormalizedRay ray = (direction ray) /= [0,0,0] ==> 1-fudgefactor <= _len && _len <= 1+fudgefactor
    where 
        _len = ((distance [0,0,0]) . direction . normalize) ray 
        fudgefactor = 0.0000000000001
        
prop_normalizeDot p1 = (direction p1) /= [0,0,0] ==> 1-fudgefactor <= _dot && _dot <= 1+fudgefactor
    where 
        np1 = normalize p1
        _dot = np1 `dot` np1
        fudgefactor = 0.0000000000001

