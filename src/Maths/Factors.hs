module Maths.Factors where

factors :: Int -> [Int] 
factors num =  [x | x <- [1 ..  num], num `mod` x == 0]

main :: IO ()
main = do
    print (factors 28)