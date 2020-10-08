module Sorts.ShellSort where


listToSort :: [Int]
listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

-- Produces the sequence of gaps to use for sorting a list of size n
gaps :: Int -> [Int]
gaps n = reverse (takeWhile (< n) tokuda)

-- Produces Tokuda's sequence
tokuda :: [Int]
tokuda = [gap n | n <- [1..]]

-- Computes term k in Tokuda's sequence
gap :: (Integral a) => a -> a
gap k = ceiling (gap' k)

-- Recursize helper function for gap
gap' :: (Integral a) => a -> Double
gap' 1 = 1
gap' k = 2.25 * gap' (k - 1) + 1

-- Sort an input sequence using the shell sort algorithm
shellSort :: (Ord a) => [a] -> [a]
shellSort xs = shellSort' xs (gaps (length xs))

-- Recursive helper function for shellSort
shellSort' :: (Ord a) => [a] -> [Int] -> [a]
shellSort' [] _ = []
shellSort' [x] _ = [x]
shellSort' l [] = l
shellSort' l (g:gs) = shellSort' (combine [insertionSort (getSub l g i) | i <- [0..g-1]]) gs

-- Get the sublist of l formed by taking elements with gap g between them, starting at index i
getSub :: (Ord a) => [a] -> Int -> Int -> [a]
getSub [] _ _ = []
getSub l g i = [l !! e | e <- [i, i+g..length l - 1]] 


-- Combine a list of sublists into a single list in the correct order
-- Required after individual sublists have been sorted, to rebuild the main list
combine :: [[a]] -> [a]
combine [] = []
combine l@(xs:_)
        | length xs == 0 = []
        | otherwise = [x | (x:_) <- l] ++ combine (map (drop 1) l)

--
-- Shell sort uses insertion sort for sorting individual sublists
-- All code below taken from Sorts.InsertionSort
--
insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = [] -- Empty list is empty
insertionSort [x] = [x] -- Singleton lists are trivially sorted.
insertionSort (x:xs) = insert x (insertionSort xs)

-- Assumes that the second argument is an alread-sorted list,
-- and inserts the first argument in the appropriate position
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x lst@(y:ys) = if x <= y then x:lst else y:(insert x ys)


main :: IO ()
main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    putStrLn $ "Sorted: " ++ show (shellSort listToSort)