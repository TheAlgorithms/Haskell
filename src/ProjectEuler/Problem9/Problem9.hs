-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2

-- For example, 32 + 42 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

module Main where

answer :: Int
answer = head [a*b*c | a <- [1..500],
                       b <- [1..500],
                       c <- [1..500], 
                     a^2 + b^2 == c^2,
                     a+b+c == 1000] 

main = do print answer