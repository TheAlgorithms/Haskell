main = do
  print $ sum $ search [10..10000000]

factOfDig :: Integer -> Integer
factOfDig x | x < 10    = fact x
            | otherwise = (+) (factOfDig $ mod x 10) (factOfDig $ quot x 10)
  where   fact 0 = 1
          fact x = x * fact (x-1)

search :: [Integer] -> [Integer]
search [] = []
search (x:xs) | x == factOfDig x = x:(search xs)
              | otherwise       = search xs