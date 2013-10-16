module HSRT.SceneReader (
  readScene
) where

import HSRT.Types
import HSRT.Renderable
import Text.ParserCombinators.Parsec

readScene :: String -> [Sphere]
readScene _ = (mksphere [0,0,10] 1.0 (Color 1 0 0)) : []
--readScene inputFile = parseFromFile sceneParser inputFile

scene = "sphere: (0,0,0) 0xff0000"

sphere :: Parser String
sphere = do 
  x <- string "sphere:" <|> string "a"
  return x
  <?> "sphere"

--sceneParser = do 
  --lines <- many items
  --return lines
