main = do
  print $ sum $ search [10..1000000]

powOfDig :: Integer -> Integer
powOfDig x  | x < 10    = x^5
            | otherwise = (+) (powOfDig $ mod x 10) (powOfDig $ quot x 10)

search :: [Integer] -> [Integer]
search [] = []
search (x:xs) | x == powOfDig x = x:(search xs)
              | otherwise       = search xs