module Main (main) where

import SVG

main :: IO ()
main = writeFile "pwm.svg" $ render $ place 20 $ scale unitsPerInch
  $              circlePWM
  ++ move (3, 0) circlePWM
  ++ move (0, 3) circlePWM
  ++ move (3, 3) circlePWM

unitsPerInch :: Double
unitsPerInch = 400 / 4.44

circlePWM :: [Shape]
circlePWM =  Circle 0 0 0.05
          :  Circle 0 0 innerRadius
          -- :  Circle 0 0 outerRadius
          :  connectDots points
          ++ connectDots [ (-x, y) | (x, y) <- points ]
  where
  innerRadius = 0.3
  outerRadius = 2
  points = map point [0, 0.01 .. 1]

  point :: Double -> (Double, Double)
  point dutyCycle = (x, y)
    where
    a = dutyCycle * pi
    r = dutyCycle * (outerRadius - innerRadius) + innerRadius
    x = r * sin a
    y = r * cos a

connectDots :: [(Double, Double)] -> [Shape]
connectDots ((x0, y0) : a@(x1, y1) : rest) = Line x0 y0 x1 y1 : connectDots (a : rest)
connectDots _ = []

