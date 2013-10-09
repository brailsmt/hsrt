module Main where

import HSRT
import HSRT.Types
import HSRT.Renderable

import System.IO
import System.Environment

render :: Image
render = renderScene (Viewport [] []) []

{- for each pixel return the PPM representation for that pixel -}
getPpmForPixel :: Color -> String
getPpmForPixel (Color r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " "

writeImage :: Handle -> [Sphere] -> IO ()
writeImage handle scene = do
  putStr "P6 512 512 255 " -- PPM header
  mapM_ (putStr . getPpmForPixel) render
  where
      putStr = hPutStr handle

main = do
    a <- getArgs
    imgHandle <- openFile "img.ppm" WriteMode
    writeImage imgHandle [] 
    hClose imgHandle
