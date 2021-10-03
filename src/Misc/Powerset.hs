module Misc.Powerset where

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x :) (powerset xs)

sampleData :: [Int]
sampleData = [1,2,3,4]

main :: IO ()
main =
    print (powerset sampleData)
