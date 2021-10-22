module ProjectEuler.Problem2.Problem2 where

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print $ sum $ filter even $ takeWhile (<=4000000) fibs
