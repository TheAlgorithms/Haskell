module ProjectEuler.Problem1.Problem1 where

solList :: [Integer]
solList = filter (\n -> (rem n 5 == 0) || (rem n 3 == 0)) [1..999]

main :: IO ()
main = do
    print $ sum solList
