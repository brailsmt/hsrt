
-- A world coordinate
type Point = [Double]
mkpt :: [Double] -> [Double]
mkpt = take 3

-- A ray of light
data Ray = Ray {
	origin    :: Point,
	direction :: Point
} deriving (Show, Eq)

-- A Sphere
data Sphere = Sphere {
	center      :: Point,
	radius      :: Double{-,
	sphereColor :: Color
	-}
} deriving (Show, Eq)

-- normalize a ray
normalize :: Ray -> Ray
normalize (Ray o d) = Ray o (map (/l) d)
    where 
        l = (sqrt . sum . (map (^2))) d

-- Euclidian distance between two points in n-space, this is a generalization of the Pythogorean Theorem
distance :: Point -> Point -> Double
distance p1 p2 = (sqrt . sum . (map (^2))) $ diff p1 p2

-- dot product
dot a b = (sum (zipWith (*) a b))

-- Subtract one point from another
diff :: Point -> Point -> Point
diff = zipWith (-) 

-- Solve the quadratic equation
-- Implement the +/- "operator".  The '-' result is listed first since it is likliest to be the value we want when ray
-- tracing
plusminus :: Double -> Double -> [Double]
plusminus x y = [x-y, x+y]

-- calculate the discriminant given A, B and C
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = (b^2) - (a * c)

-- calculate the solutions to the quadratic equation provided the A, B and discriminant terms are provided, we add the
-- optimization for a ray tracer that we only want those intersections that are >= 0  (Actually, we only want those
-- intersections that are in front of the window, but we'll deal with that later)
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b discr
    | discr < 0 = []
    | otherwise = filter (\x -> x >= 0) $ map (/a) $ (-b) `plusminus` (sqrt discr)

-- Find the intersection between a ray and a sphere
intersection :: Ray -> Sphere -> Double
intersection 
    (Ray rayOrigin rayDirection) 
    (Sphere sphereCenter sphereRadius) 
        = head $ solveQuadratic a b (discriminant a b c)
        where 
            v = diff rayOrigin sphereCenter
            a = rayDirection `dot` rayDirection
            b = v `dot` rayDirection
            c = v `dot` v - sphereRadius^2
