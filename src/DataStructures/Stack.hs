module DataStructures.Stack where

data Stack t = Empty  | Node t (Stack t) deriving(Show)

push :: Stack t -> t -> Stack t
push Empty n = (Node n Empty)
push (Node t tail) n = (Node n (Node t tail)) 

pop :: Stack t -> Stack t
pop Empty = Empty
pop (Node t tail) = tail

stackToList :: Stack t -> [t]
stackToList Empty = []
stackToList (Node t tail) = t:stackToList tail

listToStack :: [t] -> Stack t
listToStack [] = Empty
listToStack (x:xs) = aux (Node x Empty) xs
            where
                aux s [] = s
                aux s (x:xs) = aux (push s x) xs

main = do
    print (listToStack [1..5])