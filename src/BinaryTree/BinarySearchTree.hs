module BinaryTree.BinarySearchTree where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)
data Side = LeftSide | RightSide deriving (Eq, Show)

-- Function to get the data associated with the node.
nodeKey :: BTree a -> Maybe a
nodeKey Empty = Nothing
nodeKey (Node x _ _) = Just x

-- Perform inorder walk of the binary search tree.
-- Cormen, Thomas H., et al. Introduction to algorithms.  pg. 288, MIT press, 2009.
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

-- Function to find the maximum value in the BST.
bstMax :: (Eq a, Ord a) => BTree a -> Maybe a
bstMax Empty = Nothing
bstMax (Node x Empty Empty) = Just x
bstMax (Node x l Empty) = Just x
bstMax (Node x l r) = bstMax r

-- Function to find the minimum value in the BST.
bstMin :: (Eq a, Ord a) => BTree a -> Maybe a
bstMin Empty = Nothing
bstMin (Node x Empty Empty) = Just x
bstMin (Node x Empty r) = Just x
bstMin (Node x l r) = bstMin l

-- Function to build BST from a list of values using a fold.
bstFromList :: (Eq a, Ord a) => [a] -> BTree a
bstFromList [] = Empty
bstFromList lst = foldl (\tree elem -> bstInsert tree elem) Empty lst

sampleTree = bstFromList [10, 7, 3, 11, 12, 1, 3, 2]

-- Function to check if a given tree is a Binary Search Tree.
-- Property: 
--     x is a node in the BST. If y is a node in the left subtree of x then 
--     y.key <= x.key. If y is a node in the right subtree of x then
--     y.key >= x.key.
--     Cormen, Thomas H., et al. Introduction to algorithms. MIT press, 2009.
isBST :: (Ord a, Eq a) => BTree a -> Bool
isBST Empty = True
isBST (Node x Empty Empty) = True
isBST (Node x Empty r) = (x < (nkey r)) && (isBST r) where nkey = (\(Node n ll rr) -> n)
isBST (Node x l Empty) = (x >= (nkey l)) && (isBST l) where nkey = (\(Node n ll rr) -> n)
isBST (Node x l r) = (x >= (nkey l)) && (x < (nkey r)) && (isBST l) && (isBST r) where nkey = (\(Node n ll rr) -> n)