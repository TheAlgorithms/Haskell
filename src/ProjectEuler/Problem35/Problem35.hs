main = do
  print . length $ filter circular [1..1000000]

isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..(isqrt k)], k `mod` x == 0] else False
  where isqrt = round.sqrt.fromIntegral

nOfDg :: Int -> Int
nOfDg n = 1 + floor ( logBase (fromIntegral 10) (fromIntegral (n)))

rotate :: Int -> Int
rotate x = (mod x 10) * 10^(digit-1) + (quot x 10)
  where digit = nOfDg x

circular :: Int -> Bool
circular x = check x (nOfDg x)
  where check x 0 = True
        check x a | isPrime x = check (rotate x) (a-1)
                  | otherwise = False