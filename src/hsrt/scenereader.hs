module HSRT.SceneReader (
  readScene
) where

import HSRT.Types
import HSRT.Renderable.Sphere
import Text.ParserCombinators.Parsec

readScene _ = (Sphere [0,0,90] 50 (Color 1 0 0)) : []
lightSources = [LightSource [0,90,90] (Color 1 1 1)]

scene = "sphere: (0,0,0) 0xff0000"

sphere :: Parser String
sphere = do 
  x <- string "sphere:" <|> string "a"
  return x
  <?> "sphere"

--sceneParser = do 
  --lines <- many items
  --return lines
