module Misc.NQueens where
{-
The n-Queens search is a backtracking algorithm. The n-Queens problem fits n 
number of Queens onto a Chess board. As only one Queen can fit per row, a 
one-dimensional integer array is used to represent the Queen's offset on each 
row.
-}

import Data.List (permutations)

main :: IO ()
main = nqueens 8

nqueens :: Int -> IO ()
nqueens size = mapM_ (printBoard size) $ take 1 $ filter (evaluateBoard size) $ board_permutations size

-- N sized Chess boards are represented as a one-dimension array.
board_permutations :: Int -> [[Int]]
board_permutations size = permutations [0..size - 1]

-- Count the number of valid boards for a specified Chess board size.
count_boards :: Int -> Int
count_boards size = length $ filter (evaluateBoard size) $ board_permutations size

-- Show every valid board
nqueens_list :: Int -> IO ()
nqueens_list size = mapM_ (printBoard size) $ filter (evaluateBoard size) $ board_permutations size

-- Board printing function
printBoard :: Int -> [Int] -> IO ()
printBoard size board = printBoard2 board >> putStrLn ""
  where
    printBoard2 [] = return ()
    printBoard2 (row : rows) = printRow size row >> printBoard2 rows

printRow :: Int -> Int -> IO ()
printRow size row = putStrLn $ concat (lstring ++ ["Q "] ++ rstring)
  where
    lstring = (replicate row ". ")
    rstring = replicate (size - row - 1) ". "

-- Recursively check that prior rows are valid.
evaluateBoard :: (Eq a, Num a) => t -> [a] -> Bool
evaluateBoard _ [] = True
evaluateBoard size rows = (evaluateBoard size $ init rows) && validate size (init rows) (last_row - 1) (last_row + 1) last_row
  where
    last_row = last rows

-- Validate that a Queen on a row doesn't have conflicts with earlier rows.
validate :: (Eq t1, Num t1) => t2 -> [t1] -> t1 -> t1 -> t1 -> Bool
validate _ [] _ _ _ = True
validate size rows left right position
  | check_row == left     = False
  | check_row == right    = False
  | check_row == position = False
  | otherwise = validate size (init rows) (left - 1) (right + 1) position
  where
    check_row = last rows
