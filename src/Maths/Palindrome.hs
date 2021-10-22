module Maths.Palindrome where

palindrome :: Eq a => [a] -> Bool
palindrome xs = (xs == reverse xs)

main :: IO ()
main = do
  print (palindrome [1::Int, 3, 1])
