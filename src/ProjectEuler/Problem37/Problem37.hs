main = do
  print $ filter truncatable [10..1000000]
  print.sum $ filter truncatable [10..1000000]

isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..(isqrt k)], k `mod` x == 0] else False
  where isqrt = round.sqrt.fromIntegral

nOfDg :: Int -> Int
nOfDg n = 1 + floor ( logBase (fromIntegral 10) (fromIntegral (n)))

truncateL :: Int -> Int
truncateL x = (mod x (10^(digit-1)))
  where digit = nOfDg x

truncateR :: Int -> Int
truncateR x = (quot x 10)

truncatable :: Int -> Bool
truncatable x = (tableL x) && (tableR x)
  where tableL 0 = True
        tableL x  | isPrime x = tableL $ truncateL x
                  | otherwise = False
        tableR 0 = True
        tableR x  | isPrime x = tableR $ truncateR x
                  | otherwise = False 