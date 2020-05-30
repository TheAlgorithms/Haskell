module BinaryTree where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

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

-- Pretty Print a Binary Tree
simplePrint :: (Show a) => BTree a -> String
simplePrint Empty = ""
simplePrint t = (nodeShow t) ++ " " ++ (simplePrint $ getLeftTree t) ++ (simplePrint $ getRightTree t)

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
--getLevel level i (Node n l r) = (if (i == level) then [n] else (getLevel level (i+1) l)) ++ (if i == level then [n] else getLevel level (i+1) r)

-- Generate a Binary Tree from a list of values.
-- Assume list is in breadth first order.
fromList :: [a] -> BTree a
fromList lst = fromListInt 0 lst

fromListInt :: Int -> [a] -> BTree a
fromListInt _ [] = Empty
fromListInt i lst@(x:xs) = Node x (fromListInt (2*i + 1) (drop (i+1) lst)) (fromListInt (2*i + 2) (drop (i+2) lst))