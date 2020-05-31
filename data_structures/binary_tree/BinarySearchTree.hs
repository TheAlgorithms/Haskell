module BinarySearchTree where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)
data Side = LeftSide | RightSide deriving (Eq, Show)

-- Function to get the data associated with the node.
nodeKey :: BTree a -> Maybe a
nodeKey Empty = Nothing
nodeKey (Node x _ _) = Just x

inorderWalk :: (Eq a, Ord a) => BTree a -> [a]
inorderWalk Empty = []
inorderWalk (Node x l r) = (inorderWalk l) ++ [x] ++ (inorderWalk r)

-- Function to insert a value into the tree. Returns the new tree.
-- Cormen, Thomas H., et al. Introduction to algorithms.  pg. 294, MIT press, 2009.
bstInsert :: (Eq a, Ord a) => BTree a -> a -> BTree a
bstInsert Empty z = Node z Empty Empty
bstInsert (Node x l r) z
    | z < x = Node x (bstInsert l z) r
    | otherwise = Node x l (bstInsert r z)

-- Function to check if a given tree is a Binary Search Tree.
-- Property: 
--     x is a node in the BST. If y is a node in the left subtree of x then 
--     y.key <= x.key. If y is a node in the right subtree of x then
--     y.key >= x.key.
--     Cormen, Thomas H., et al. Introduction to algorithms. MIT press, 2009.
-- isBST :: (Ord a, Eq a) => BTree a -> Bool
-- isBST (Node x l r) = (x >= (nodeKey l)) && (x <= (nodeKey r)) && (isBST l) && (isBST r)
--                      where condition1 = 