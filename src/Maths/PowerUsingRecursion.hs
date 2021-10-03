module Maths.PowerUsingRecursion where

-- Raise a base to the power of the exponent using recursion

power :: Int -> Int -> Int 
power _ 0 = 1
power b n | n > 0 = b * power b (n -1)
          | otherwise = error "Integer exponentiation not defined for negative exponents"
