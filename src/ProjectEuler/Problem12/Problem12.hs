module ProjectEuler.Problem12.Problem12 where

-- Definition of a triangle number
triangleNumbers :: [Int]
triangleNumbers = scanl1 (+) [1..]

-- Returns the number of divisors for a number n
divisors :: Integral a => a -> [a]
divisors n = concatMap f (filter ((==) 0 . mod n) (takeWhile (\k -> k*k <= n) [1 .. n]))
    where f k | k < l     = [k, l]
              | otherwise = [k]
              where 
                  l = div n k

-- If the number of divisors is less than 500, then recurse up
solve :: Int -> Int 
solve index 
    | x >= 500 = triangleNumbers !! index
    | x <  500 = solve (index + 1)
    where x = length $ divisors (triangleNumbers !! index)

main :: IO ()
main = do print (solve 1)