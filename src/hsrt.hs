module HSRT where

import System.Environment
import System.IO
import Data.List

import HSRT.Types
import HSRT.Renderable

camera :: Ray
camera = Ray [0,0,0] [0,0,1]

scene :: [Sphere]
scene = [mksphere [0,0,20] 1 defaultColor, mksphere [0.5,0,5] 1 defaultColor]

renderScene :: Viewport -> [Sphere] -> Image
renderScene v _ = Image v [(Color (x/w) (x/w) (x/w)) | x<-[0..w],y<-[0..h]]
    where
        w = width v
        h = height v
