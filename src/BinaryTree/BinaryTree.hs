module BinaryTree.BinaryTree where

import qualified Data.List as L

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)
data Side = LeftSide | RightSide deriving (Eq, Show)

-- Get subtree on specified side
getSubTree :: Side -> BTree a -> BTree a
getSubTree _ Empty = Empty
getSubTree s (Node _ l r) = if s == LeftSide then l else r

-- Get Left Subtree
getLeftTree :: BTree a -> BTree a
getLeftTree Empty = Empty
getLeftTree (Node _ l _) = l

-- Get Right Subtree
getRightTree :: BTree a -> BTree a
getRightTree Empty = Empty
getRightTree (Node _ _ r) = r

-- Get string representation of node Data
nodeShow :: (Show a) => BTree a -> String
nodeShow Empty = ""
nodeShow (Node val _ _) = show val

-- Depth first traversal
dfsList :: BTree a -> [a]
dfsList Empty = []
dfsList (Node n l r) = [n] ++ (dfsList l) ++ (dfsList r)

-- Breadth first traversal.
bfsList :: BTree a -> [a]
bfsList Empty = []
bfsList t = concat $ takeWhile (\l -> (length l) > 0) [getLevel i 0 t | i <- [0..]]

-- Get all nodes from a single level in the tree.
getLevel :: (Num b, Enum b, Eq b) => b -> b -> BTree a -> [a]
getLevel _ _ Empty = []
getLevel 0 _ (Node n l r) = [n]
getLevel level i (Node n l r)
    | i == level = [n]
    | otherwise = (getLevel level (i+1) l) ++ (getLevel level (i+1) r)

-- Get a list of lists of nodes in each level
getLevels :: BTree a -> [[a]]
getLevels t = takeWhile (\l -> (length l) > 0) [getLevel i 0 t | i <- [0..]]

-- Get the depth of the tree
getDepth :: BTree a -> Int
getDepth t = length $ getLevels t

-- Generate a Binary Tree from a list of values.
-- Assume list is in breadth first order.
fromList :: [a] -> BTree a
fromList lst = fromListInt 0 lst
-- Internal function to convert list to tree.
fromListInt :: Int -> [a] -> BTree a
fromListInt _ [] = Empty
fromListInt i lst@(x:xs) = Node x (fromListInt (2*i + 1) (drop (i+1) lst)) 
                                  (fromListInt (2*i + 2) (drop (i+2) lst))

-- Count number of nodes in the tree.
numNodes :: BTree a -> Int
numNodes t = length $ bfsList t

-- Pretty Print a Binary Tree
simplePrint :: (Show a) => BTree a -> String
simplePrint Empty = ""
simplePrint t = (nodeShow t) ++ " " ++ (simplePrint $ getLeftTree t) ++ (simplePrint $ getRightTree t)

-- Find count of element occurrence in binary tree
elementCountInTree :: (Ord a, Eq a) => BTree a -> a -> Int
elementCountInTree Empty _ = 0
elementCountInTree (Node x l r) ele =
    (if ele == x then 1 else 0) + 
    elementCountInTree l ele    +
    elementCountInTree r ele

-- Find whether tree is symmetric at root
-- Uses Haskell Eq instance to compare BTree datatype
isSymmetric :: (Eq a) => BTree a -> Bool
isSymmetric Empty = True
isSymmetric (Node _ l r) = l == r

-- Get sum of all elements in tree
sumTree :: BTree Int -> Int
sumTree Empty = 0
sumTree (Node x l r) = x + sumTree l + sumTree r 

-- Get an array of leaf nodes in a binary tree
getLeafNodes :: (Eq a, Show a) => BTree a -> [a]
getLeafNodes Empty = []
getLeafNodes (Node x l r)
    | l == Empty && r == Empty = [x]
    | otherwise = getLeafNodes l ++ getLeafNodes r