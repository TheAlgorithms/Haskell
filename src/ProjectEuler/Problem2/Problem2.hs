module ProjectEuler.Problem2.Problem2 where

fib :: Integer -> [Integer]
fib n
    | n < 0 = []
    | n == 1 = [0]
    | n == 2 = [0, 1]
    | otherwise = reverse $ foldl (\acc n -> (sum (take 2 acc)):acc) [1, 0] [3..n]

main = do
    print $ sum $ filter even $ takeWhile (<=4000000) (fib 100)