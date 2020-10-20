module Maths.GraphDist where

-- calculates the distance from the origin
calcDistOrigin :: Floating a => a -> a -> a -> a
calcDistOrigin x y z = sqrt ((x ** 2) + (y ** 2) + (z ** 2))

-- calculates the distance between two points
calcDist :: Floating a => a -> a -> a -> a -> a -> a -> a
calcDist x1 x2 y1 y2 z1 z2 = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

main :: IO ()
main = do
  print (calcDistOrigin 4 5 6)
  print (calcDist 4 6 5 9 6 10)