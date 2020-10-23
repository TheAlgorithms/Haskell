module Misc.Powerset where

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (powerset xs) ++ (map (\ys -> x:ys) (powerset xs)) 

xs = [1,2,3,4]

main :: IO ()
main = do
    putStrLn (show (powerset xs))
