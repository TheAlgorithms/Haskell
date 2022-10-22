module ProjectEuler.Problem9.Problem9 where

-- Brute force approach using list comprehensions
answer :: Int
answer = head [a*b*c | a <- [1..500],
                       b <- [1..500],
                       c <- [1..500], 
                     a^2 + b^2 == c^2, -- Filters
                     a+b+c == 1000]    -- Filters

main = do print answer