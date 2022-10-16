main = do 
  print $ sumDigit $ fact 100

sumDigit :: Integer -> Integer
sumDigit a | a >= 10    = (+) (a `mod` 10) $ sumDigit (quot a 10)
           | otherwise  = a


fact :: Integer -> Integer
fact 1 = 1
fact x = x * fact (x-1)