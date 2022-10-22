-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

module Main where

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && null [() | k <- [2 .. floor $ sqrt $ fromIntegral n], mod n k == 0]

answer :: Integer
answer = sum $ filter isPrime [1..2000000]

main = do print answer