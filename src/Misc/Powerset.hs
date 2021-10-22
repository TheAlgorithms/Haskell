module Misc.Powerset where

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

test_list :: [Int]
test_list = [1,2,3,4]

main :: IO ()
main = print $ powerset test_list
