module Misc.NQueens where
{-
The n-Queens search is a backtracking algorithm. The n-Queens problem fits n 
number of Queens onto a Chess board. As only one Queen can fit per row, a 
one-dimensional integer array is used to represent the Queen's offset on each 
row.
-}

import Data.List (permutations)

main = nqueens 8
nqueens size = mapM_ (printBoard size) $ take 1 $ filter (evaluateBoard size) $ board_permutations size

--N sized Chess boards are represented as a one-dimension array.
board_permutations size = permutations [0..size - 1]

--Count the number of valid boards for a specified Chess board size.
count_boards size = length $ filter (evaluateBoard size) $ board_permutations size

--Show every valid board 
nqueens_list size = mapM_ (printBoard size) $ filter (evaluateBoard size) $ board_permutations size

--Board printing function
printBoard size board = do 
    printBoard2 size board 
    putStrLn "" where
        printBoard2 _ [] = return ()
        printBoard2 size board = do
            let row = head board
            printRow size row
            printBoard2 size $ tail board

printRow size row = do
    let lstring = (replicate row ". ")
    let rstring = replicate (size - row - 1) ". "
    putStrLn $ concat (lstring ++ ["Q "] ++ rstring)
    return ()

--Recursively check that prior rows are valid.
evaluateBoard _ [] = True
evaluateBoard size rows = (evaluateBoard size $ cut_last rows) && validate size (cut_last rows) (last_row - 1) (last_row + 1) last_row where
    last_row = last rows

--Validate that a Queen on a row doesn't have conflicts with earlier rows.
validate _ [] _ _ _ = True
validate size rows left right position = if check_row == left || check_row == right || check_row == position then False else validate size (cut_last rows) (left - 1) (right + 1) position where
    check_row = last rows

cut_last x = reverse $ drop 1 $ reverse x
