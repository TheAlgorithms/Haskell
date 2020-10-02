module Sorts.SelectionSort where

listToSort :: [Int]
listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]


-- The selection sort function
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) =
    let (y, ys) = leastUnsorted (x:xs)
    in y : selectionSort ys

-- select least element from unsorted list, return it and the rest of unsorted list
leastUnsorted :: (Ord a) => [a] -> (a, [a])
leastUnsorted [x] = (x, [])
leastUnsorted (x:xs) =
    let (y, ys) = leastUnsorted xs
    in if x <= y then (x, xs) else (y, x:ys)

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (selectionSort listToSort)