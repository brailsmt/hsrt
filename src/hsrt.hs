module HSRT where

import System.Environment
import System.IO
import Data.List

import HSRT.Types
import HSRT.Renderable.Sphere

camera :: Ray
camera = Ray [0,0,0] [0,0,1]

--scene :: Scene a
--scene = [mksphere [0,0,20] 1 defaultColor, mksphere [0.5,0,5] 1 defaultColor]

