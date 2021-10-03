module Maths.EuclidianAlgorithm where

-- Computes the Greatest Common Divisor (GCD) of two numbers using the Euclidian Algorithm
ea :: (Integral a) => a -> a -> a
ea x 0 = x
ea x y = gcd y (x `mod` y)

-- Computes the modular multiplicative inverse of a modulo m using the Extended Euclidian Algorithm
-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
eea :: (Integral a) => a -> a -> Maybe a
eea a m = let (r, t) = eea' 0 m 1 a
              in if r > 1 then Nothing else if t < 0 then Just (t + m) else Just t

-- The looping part of the algorithm, implemented recursively
eea' :: (Integral a) => a -> a -> a -> a -> (a, a)
eea' t0 r0 t1 r1
    | r1 /= 0 = let q = r0 `div` r1
                    in eea' t1 r1 (t0 - q * t1) (r0 - q * r1)
    | otherwise = (r0, t0)