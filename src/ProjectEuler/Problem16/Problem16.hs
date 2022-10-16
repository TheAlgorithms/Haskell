main = do 
  print $ sumDigit $ pow 2 1000

sumDigit :: Integer -> Integer
sumDigit a | a >= 10    = (+) (a `mod` 10) $ sumDigit (quot a 10)
           | otherwise  = a


pow :: Integer -> Int -> Integer
pow _ 0 = 1
pow a 1 = a
pow a 2 = a * a
pow a b | b `mod` 2 == 0  = pow (pow a (quot b 2)) 2
        | otherwise       = (pow a (b-1)) * a