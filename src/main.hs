module Main where

import HSRT
import HSRT.Types

import System.IO
import System.Environment
import Data.List
import Data.List.Split


--intersection = head $ sortIntersections $ (observe "findIntersections" (findIntersections)) (head rays) scene

objects = [Sphere [0,0,1] 1 (Color 0 0 1), Sphere [0,1,0] 1 (Color 0 1 0), Sphere [1,0,0] 1 (Color 1 0 0)]
lightSources  = [LightSource [0,1000,0] white]

-- Print a PPM header for a P3 PPM image.
ppmHeader :: Image -> String
ppmHeader (Image vp _) = "P3 " ++ (show $ width vp) ++ " " ++ (show $ height vp) ++ " " ++ (show $ ppmMaxColorValue) ++ "\n"

-- Convert an image to PPM data.
ppmData :: Image -> [String]
ppmData (Image vp imgdata) = intercalate ["\n"] (chunksOf (floor $ width vp) (map (show) imgdata))

-- Write an image to a file handle
writeImage :: Handle -> Image -> IO ()
writeImage handle image = do
  putStr $ ppmHeader image 
  mapM_ (putStr) $ ppmData image
  where
    putStr    = hPutStr handle

-- Accepts 3 arguments:  the output filename, width and heigth
main = do
  args <- getArgs
  imgHandle <- openFile (fname args) WriteMode
  writeImage imgHandle $ rndr (width args) (height args) 
  hClose imgHandle
    where 
      fname  = head 
      width  = (read . head . tail)
      height = (read . head . tail . tail)
      rndr w h = renderScene (Scene (mkviewport w h 1) objects lightSources)
