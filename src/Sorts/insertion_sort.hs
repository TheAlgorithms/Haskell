import Data.List (minimum, delete)

-- Insertion sort 
listToSort :: [Int]
listToSort = [33, 21, 3, 223, 17, 3, 1, 5, 566, 12, 500, 10, 15, 89, 7, 11, 18, 19, 6, 20]

-- the insertion sort function
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
           

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (isort listToSort)
