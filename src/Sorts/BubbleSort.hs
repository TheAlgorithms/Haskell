module Sorts.BubbleSort where

listToSort :: [Int]
listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]


-- The bubble sort function
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort lst = if bpassed == lst then lst
                 else bubbleSort bpassed
                 where bpassed = bubblePass lst

-- A single pass of bubble sort
bubblePass :: (Ord a) => [a] -> [a]
bubblePass [] = [] -- Empty list is empty.
bubblePass [x] = [x] -- Singleton list is always trivially sorted.
bubblePass (x1:x2:xs) = if x1 > x2
                        then [x2] ++ (bubblePass ([x1] ++ xs))
                        else [x1] ++ (bubblePass ([x2] ++ xs))

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (bubbleSort listToSort)