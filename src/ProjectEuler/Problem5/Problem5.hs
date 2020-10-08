module ProjectEuler.Problem5.Problem5 where

import Data.List

primeFactors' :: Integer -> Integer -> [Integer]
primeFactors' _ 1 = [1]
primeFactors' d n
  | d * d > n       = [n]
  | n `rem` d == 0  = d : primeFactors' d (n `div` d)
  | otherwise       = primeFactors' (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors x = primeFactors' 2 x

combinedPrimeFactorisationTo :: Integer -> [Integer]
combinedPrimeFactorisationTo n
  | n == 1    = [1]
  | otherwise = furtherPrimes ++ (nPrimes \\ furtherPrimes)
      where
        furtherPrimes = combinedPrimeFactorisationTo (n - 1)
        nPrimes       = primeFactors n

main :: IO ()
main = do
  print $ product $ combinedPrimeFactorisationTo 20
