module Maths.PerfectNumber where

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

perfectNumber :: Int -> Bool
perfectNumber num = sumList [ x | x <- [1 ..  num -1], num `mod` x == 0] == num

main = do
    print (perfectNumber 6)