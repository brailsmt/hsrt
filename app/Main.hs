module Main where 

-- Haskell Ray Tracer
import System.Environment
import System.IO
import HSRT

spheres = [(Sphere (Point 0 (0-50) (300)) 20.0 (Color 1 1 0))]
_lights = [(Point 0 0 (-100))]
scene = Scene spheres _lights

-- Print a PPM header for a P3 PPM image.
ppmHeader :: Integer ->Integer ->String
ppmHeader width height = "P3 " ++ (show width) ++ " " ++ (show height) ++ " 255"

writeImage :: String ->(Integer, Integer) ->Image ->IO ()
writeImage filename vp image = do
    imgHandle <- openFile filename WriteMode
    (hPutStrLn imgHandle) $ ppmHeader (fst vp) (snd vp)
    mapM_ ((hPutStrLn imgHandle) . show) image

main :: IO ()
main = do
    args <- getArgs
    writeImage (head args) ((width args), (height args)) (traceScene scene ((width args), (height args)) (dist args))
        where
            width  = (read . head . tail)
            height = (read . head . tail . tail)
            dist a = read ((head . tail . tail . tail) a) :: Double

