module ProjectEuler.Problem7.Problem7 where

primes :: [Integer]
primes = sieve [2..]
    where
      sieve [] = []
      sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

main :: IO ()
main = do
    print $ primes !! 10000 -- zero indexing
