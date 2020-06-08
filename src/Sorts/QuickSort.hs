module Sorts.QuickSort where

listToSort :: [Int]
listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] -- Empty list is empty.
quicksort [x] = [x] -- Singleton list is always trivially sorted.
quicksort [x, y] = [(min x y), (max x y)]
quicksort (x:xs) =
  quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]
  -- x is the pivot, left quicksort returns smaller sorted and right quicksort bigger sorted

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (quicksort listToSort)

