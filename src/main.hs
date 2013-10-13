module Main where

import HSRT
import HSRT.Types
import HSRT.Renderable

import System.IO
import System.Environment
import Data.List.Split

ppmMaxColorValue = 255

readScene = []

render :: Double -> Double -> [Sphere] -> Image
render w h _ = renderScene (Viewport w h) []

{- for each pixel return the PPM representation for that pixel -}
getPpmForPixel :: Color -> String
getPpmForPixel (Color red green blue) = r ++ " " ++ g ++ " " ++ b ++ "  "
    where 
        r = show $ round $ red * ppmMaxColorValue
        g = show $ round $ green * ppmMaxColorValue
        b = show $ round $ blue * ppmMaxColorValue

{- Break a list of colors into scanlines of width length -}
--toScanlines :: Int -> Image -> [Image]
--toScanlines width img = chunksOf width img

ppmHeader :: Image -> String
ppmHeader (Image (Viewport w h) _) = "P3 " ++ (show $ floor w) ++ " " ++ (show $ floor h) ++ " " ++ (show $ ppmMaxColorValue) ++ "\n"

ppmData :: Image -> [String]
ppmData (Image (Viewport w h) imgdata) = map (getPpmForPixel) imgdata

writeImage :: Handle -> Image -> IO ()
writeImage handle image = do
  putStr $ ppmHeader image 
  mapM_ (putStr) $ ppmData image
  where
    putStr    = hPutStr handle
    --rows      = map unwords clrs -- This is now a [String]
    --clrs      = map (map (getPpmForPixel)) scanlines  -- This is now a [[String]]
    --scanlines = toScanlines (floor w) $ render w h

main = do
  args <- getArgs
  imgHandle <- openFile (fname args) WriteMode
  writeImage imgHandle $ render (width args) (height args) (readScene)
  hClose imgHandle
    where 
      fname  = head 
      width  = (read . head . tail)
      height = (read . head . tail . tail)
