module Sorts.InsertionSort where

listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = [] -- Empty list is empty
insertionSort [x] = [x] -- Singleton lists are trivially sorted.
insertionSort (x:xs) = insert x (insertionSort xs)

-- Assumes that the second argument is an alread-sorted list,
-- and inserts the first argument in the appropriate position
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x lst@(y:ys) = if x <= y then x:lst else y:(insert x ys)


main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (insertionSort listToSort)
