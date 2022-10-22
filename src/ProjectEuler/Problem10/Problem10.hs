module ProjectEuler.Problem10.Problem10 where

-- Is a given integer n a prime?
isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && null [() | k <- [2 .. floor $ sqrt $ fromIntegral n], mod n k == 0]

-- Sum up all the primes between 1 and 2 million
answer :: Integer
answer = sum $ filter isPrime [1..2000000]

main = do print answer