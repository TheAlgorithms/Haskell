module ProjectEuler.Problem1.Problem1 where

main :: IO ()
main = print $ sum [x | x <- [(1::Int) .. 999], x `rem` 5 == 0, x `rem` 3 == 0]
