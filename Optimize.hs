module Optimize
  ( minimize
  ) where

-- | Minimization of a function given lower and upper bounds and a tollerance.
minimize :: (Double -> Double) -> (Double, Double) -> Double -> Double
minimize f (lower, upper) tollerance = m3 (lower, f lower) (ave lower upper, f $ ave lower upper) (upper, f upper)
  where
  m3 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
  m3 (x1, y1) (x3, y3) (x5, y5)
    | abs (x2 - x4) < tollerance = x3
    | y2 < y4                    = m3 (x1, y1) (x2, y2) (x3, y3)
    | otherwise                  = m3 (x3, y3) (x4, y4) (x5, y5)
    where
    x2 = ave x1 x3
    y2 = f x2
    x4 = ave x3 x5
    y4 = f x4

ave :: Double -> Double -> Double
ave a b = (a + b) / 2

