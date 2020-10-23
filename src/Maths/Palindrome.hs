module Maths.Palindrome where

palinedrome :: Eq a => [a] -> Bool
palinedrome xs = (xs == reverse xs)

main :: IO ()
main = do
  print (palinedrome [1, 3, 1])