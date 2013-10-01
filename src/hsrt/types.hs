module HSRT.Types where

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
