module Main (main) where

import Text.Printf

import Optimize
import qualified SVG as SVG

data Point = Point Double Double
data Line  = Line  Point  Point
data Wheel = Wheel Double Point

class Transform a where
  move   :: Double -> Double -> a -> a
  rotate :: Double -> a -> a

instance Transform Point where
  move x' y' (Point x y) = Point (x + x') (y + y')
  rotate a (Point x y) = Point (r * cos a') (r * sin a')
    where
    r = sqrt $ x ^ 2 + y ^ 2
    a' = atan2 y x + a

instance Transform Line where
  move x y (Line p1 p2) = Line (move x y p1) (move x y p2)
  rotate a (Line p1 p2) = Line (rotate a p1) (rotate a p2)

instance Transform Wheel where
  move x y (Wheel r p) = Wheel r $ move x y p
  rotate a (Wheel r p) = Wheel r $ rotate a p

instance Transform a => Transform [a] where
  move x y a = map (move x y) a
  rotate a b = map (rotate a) b

data Trans
  = Id
  | Move Double Double Trans
  | Rotate Double Trans

reverseTransform :: Trans -> Trans
reverseTransform = f Id
  where
  f :: Trans -> Trans -> Trans
  f a b = case b of
    Id -> a
    Move x y b -> f (Move (-x) (-y) a) b
    Rotate ang b -> f (Rotate (-ang) a) b
  
apply :: Transform a => Trans -> a -> a
apply a b = case a of
  Id -> b
  Move x y a -> move x y $ apply a b
  Rotate ang a -> rotate ang $ apply a b

segment :: Wheel -> Wheel -> (Double, Line, Double)
segment w1@(Wheel r1 _) w2@(Wheel r2 _)
  | rB == rS  = rev $ Line (Point 0 (-rB)) (Point a (-rB))
  | r1Bigger  = rev $ Line (Point (rB * cos phi) ((-rB) * sin phi)) (Point (a + rS * cos phi) ((-rS) * sin phi))
  | otherwise = rev $ Line (Point (rB * cos phi) (  rB  * sin phi)) (Point (a + rS * cos phi) (  rS  * sin phi))
  where
  r1Bigger = r1 >= r2
  (Wheel rB (Point x1 y1), wS@(Wheel rS _)) = if r1Bigger then (w1, w2) else (w2, w1)

  -- Move bigger wheel to origin.
  t1 = Move (-x1) (-y1) Id
  Wheel _ (Point x2 y2) = apply t1 wS
  a = sqrt $ x2 ^ 2 + y2 ^ 2
  b = a * rS / (rB - rS)
  phi = acos $ rS / b
  -- Rotate smaller wheel to x axis.
  t2 = Rotate (- (atan2 y2 x2)) t1
  rev = apply $ reverseTransform t2

segments :: [Wheel] -> [Line]
segments a = f a
  where
  first = head a
  f :: [Wheel] -> [Line]
  f a = case a of
    [] -> undefined
    [b] -> [segment b first]
    a : b : c -> segment a b : f (b : c)

beltLength :: [Wheel] -> Double
beltLength wheels = sum $ map lineLength segs  -- XXX Need to add arc lengths.
  where
  segs = segments wheels

lineLength :: Line -> Double
lineLength (Line (Point x1 y1) (Point x2 y2)) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

wheelRadBig   = 1.250 / 2
wheelRadMed   = 0.855 / 2
wheelRadSmall = 0.425 / 2

groundRollers :: Point -> [Wheel]
groundRollers (Point x' y) = [ Wheel r $ Point (x + d * i) y | i <- [0 .. 3] ]
  where
  x = x' - d - d / 2
  r = wheelRadSmall
  d = 0.8

wheelToSVG :: Wheel -> SVG.Shape
wheelToSVG (Wheel r (Point x y)) = SVG.Circle x (-y) r

lineToSVG :: Line -> SVG.Shape
lineToSVG (Line (Point x1 y1) (Point x2 y2)) = SVG.Line x1 (-y1) x2 (-y2)

data LoaderConfig = LoaderConfig
  { lcGroundWheelsDown
  , lcWheelBase
  , lcDriveUp
  , lcDriveBack :: Double
  }

loaderConfig a = LoaderConfig 
  { lcGroundWheelsDown = wheelRadBig - wheelRadSmall
  , lcWheelBase        = 5
  , lcDriveUp          = a
  , lcDriveBack        = 0.40
  }

loaderWheels :: LoaderConfig -> [Wheel]
loaderWheels lc =
  [ Wheel wheelRadBig $ Point 0 0 ] ++
  groundRollers (Point (lcWheelBase lc / 2) (- (lcGroundWheelsDown lc))) ++
  [ Wheel wheelRadBig $ Point (lcWheelBase lc) 0
  , Wheel wheelRadBig $ Point (lcWheelBase lc + lcDriveBack lc) (lcDriveUp lc)
  ]

f :: Double -> Double
f a = abs $ 13 - (beltLength $ loaderWheels $ loaderConfig a)

main :: IO ()
main = do
  writeFile "test.svg" $ SVG.render $ SVG.place 20 $ SVG.scale 70 $
    map wheelToSVG wheels ++ map lineToSVG (segments wheels)
  printf "Belt length: %.2f\n" $ beltLength wheels
  printf "DriveUp param: %.2f\n" a
  where
  a = minimize f (1.5, 4.5) 0.00001
  wheels = loaderWheels $ loaderConfig a

