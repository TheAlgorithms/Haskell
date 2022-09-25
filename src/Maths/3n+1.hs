module Maths.Collatz where
-- 3n + 1 is also known as the collatz conjecture, hence the function/module name
-- To avoid numbers in the names
collatz :: Integer -> [Integer]
collatz n 
    | n < 1 = undefined
    -- If n is one, the 4, 2, 1 loop has completed, therefore the function returns
    | n == 1 = [1]
    -- If n is even, prepend n to the collatz function applied to n/2
    --(using the div function for integer division)
    | n `mod` 2 == 0 = n : collatz (n `div` 2)
    -- Otherwise n is odd, prepend n to the function applied to 3n + 1
    | otherwise = n : collatz (3 * n + 1)

main :: IO()
main = do
    print $ collatz 11
    print $ length $ collatz 11

