module Sorts.MergeSort where

listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = [] -- Empty list is empty
mergeSort [x] = [x] -- Singleton lists are trivially sorted.
mergeSort [x, y] = [(min x y), (max x y)]
mergeSort lst = merge (mergeSort leftL) (mergeSort rightL)
                where leftL = take splitPoint lst
                      rightL = drop splitPoint lst
                      splitPoint = (length lst) `div` 2

-- Function to execute a merge of two sorted lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge lst1@(x:xs) lst2@(y:ys) = if x < y 
                                then x:(merge xs lst2)
                                else y:(merge lst1 ys)

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (mergeSort listToSort)