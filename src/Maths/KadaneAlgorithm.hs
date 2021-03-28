module Maths.KadaneAlgorithm where

-- Sample Input List 
inputList :: [Integer]
inputList = [1, -2, 3, 4, -5, 6, -7, 8, 9, 10]


-- Helper function that updates values of maxSoFar and maxEndingHere
-- and call itself recursively over the values of the list 
-- and when the list is empty, return the maxSoFar value
-- with the start and end indices.
maxSubArrayHelper :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer] -> (Integer, Integer, Integer)
maxSubArrayHelper maxSoFar _ _ start end [] = (maxSoFar, start, end)
maxSubArrayHelper maxSoFar maxEndingHere i start end (x:xs) = 
    let i' = i + 1
        maxEndingHere' = maxEndingHere + x
    in 
        if maxSoFar < maxEndingHere' then 
            maxSubArrayHelper maxEndingHere' maxEndingHere' i' start i xs
        else if maxEndingHere' < 0 then
            maxSubArrayHelper maxSoFar 0 i' i' i' xs
        else 
            maxSubArrayHelper maxSoFar maxEndingHere' i' start end xs

-- Initially maxSoFar (maximum sum till the previous iteration), 
-- maxEndingHere (maximum sum till end index of the current iteration),
-- start (start index) and end (end index) are sent as 0
maxSubArray :: [Integer] -> (Integer, Integer, Integer)
maxSubArray = maxSubArrayHelper 0 0 0 0 0

-- Outputs (sum, start, end)
-- sum - sum from start to end indices of the input array
-- start:end - the subarray with max sum
main :: IO ()
main = do
    print (maxSubArray inputList)