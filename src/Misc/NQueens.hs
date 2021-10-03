module Misc.NQueens where
{-
The n-Queens search is a backtracking algorithm. The n-Queens problem fits n 
number of Queens onto a Chess board. As only one Queen can fit per row, a 
one-dimensional integer array is used to represent the Queen's offset on each 
row.
-}

import Data.List (permutations)

main :: IO ()
main = nQueens 8

nQueens :: Int -> IO ()
nQueens size = mapM_ (printBoard size) $ take 1 $ filter (evaluateBoard size) $ boardPermutations size

--N sized Chess boards are represented as a one-dimension array.
boardPermutations :: (Num a, Enum a) => a -> [[a]]
boardPermutations size = permutations [0..size - 1]

--Count the number of valid boards for a specified Chess board size.
countBoards :: (Eq a, Num a, Enum a) => a -> Int
countBoards size = length $ filter (evaluateBoard size) $ boardPermutations size

--Show every valid board 
nQueensList :: Int -> IO ()
nQueensList size = mapM_ (printBoard size) $ filter (evaluateBoard size) $ boardPermutations size

--Board printing function
printBoard :: Int -> [Int] -> IO ()
printBoard size board = do
    printBoard2 size board
    putStrLn "" where
        printBoard2 _ [] = return ()
        printBoard2 size board = do
            let row = head board
            printRow size row
            printBoard2 size $ tail board

printRow :: Int -> Int -> IO ()
printRow size row = do
    let lstring = replicate row ". "
    let rstring = replicate (size - row - 1) ". "
    putStrLn $ concat (lstring ++ ["Q "] ++ rstring)
    return ()

--Recursively check that prior rows are valid.
evaluateBoard :: (Eq a, Num a) => t -> [a] -> Bool
evaluateBoard _ [] = True
evaluateBoard size rows = evaluateBoard size (init rows) && validate size (init rows) (last_row - 1) (last_row + 1) last_row where
    last_row = last rows

--Validate that a Queen on a row doesn't have conflicts with earlier rows.
validate :: (Eq b, Num b) => a -> [b] -> b -> b -> b -> Bool
validate _ [] _ _ _ = True
validate size rows left right position = not (check_row == left || check_row == right || check_row == position) && validate size (init rows) (left - 1) (right + 1) position where
    check_row = last rows
