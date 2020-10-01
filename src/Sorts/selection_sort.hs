import Data.List (minimum, delete)

-- Selection sort 
listToSort :: [Int]
listToSort = [33, 21, 3, 223, 17, 3, 1, 5, 566, 12, 500, 10, 15, 89, 7, 11, 18, 19, 6, 20]

-- the selection sort function
ssort :: Ord t => [t] -> [t]
ssort [] = []
ssort xs = let { x = minimum xs } 
           in  x : ssort (delete x xs)
           

main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (ssort listToSort)
