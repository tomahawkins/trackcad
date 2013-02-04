module SVG
  ( SVG
  , Shape (..)
  , render
  , place
  , scale
  ) where

import Data.List (intercalate)

render :: SVG -> String
render shapes = unlines $
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" zoomAndPan=\"magnify\">" ] ++
  [ "  " ++ renderShape s | s <- shapes ] ++
  [ "</svg>" ]

renderShape :: Shape -> String
renderShape a = case a of
  Circle x y r -> tag "circle" [D "cx" x, D "cy" y, D "r" r, S "stroke" "black", S "stroke-width" "1", S "fill" "none"]
  Line x1 y1 x2 y2 -> tag "line" [D "x1" x1, D "y1" y1, D "x2" x2, D "y2" y2, S "stroke" "black", S "stroke-width" "1"]

tag :: String -> [Param] -> String
tag name params = "<" ++ name ++ " " ++ intercalate " " (map show params) ++ "/>"

data Param
  = S String String
  | D String Double

instance Show Param where
  show a = case a of
    S n v -> n ++ "=\"" ++      v ++ "\""
    D n v -> n ++ "=\"" ++ show v ++ "\""

type SVG = [Shape]

data Shape
  = Circle Double Double Double        -- ^ x y radius 
  | Line   Double Double Double Double -- ^ x1 y1 x2 y2

place :: Double -> SVG -> SVG
place border shapes = map shift shapes
  where
  (xMins, yMins) = unzip $ map mins shapes
  xMin = minimum $ xMins
  yMin = minimum $ yMins
  fx x = x - xMin + border
  fy y = y - yMin + border

  mins :: Shape -> (Double, Double)
  mins a = case a of
    Circle x y r -> (x - r, y - r)
    Line x1 y1 x2 y2 -> (min x1 x2, min y1 y2)

  shift :: Shape -> Shape
  shift a = case a of
    Circle x y r -> Circle (fx x) (fy y) r
    Line x1 y1 x2 y2 -> Line (fx x1) (fy y1) (fx x2) (fy y2)

scale :: Double -> SVG -> SVG
scale s = map f 
  where
  f a = case a of
    Circle x y r -> Circle (s * x) (s * y) (s * r)
    Line x1 y1 x2 y2 -> Line (s * x1) (s * y1) (s * x2) (s * y2)

