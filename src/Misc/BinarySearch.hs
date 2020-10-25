module Misc.BinarySearch where

bsWithIndex :: (Ord a) => [a] -> a -> Int -> Maybe Int
bsWithIndex list n i
    | n == head list        = Just i
    | len == 1              = Nothing   -- only candidate in list is not the right elem
    | n < head ys           = bsWithIndex xs n i
    | otherwise             = bsWithIndex ys n (i + half)
    where 
        len         = length list
        half        = len `div` 2 
        (xs, ys)    = splitAt half list

bs :: (Ord a) => [a] -> a -> Int
bs list n = case bsWithIndex list n 0 of
    Just x  -> x
    Nothing -> -1

main :: IO ()
main = do
    let intList = [1,4,7,10,25,30]
    print $ bs intList 29  -- 29 -> -1 as not in list
    print $ bs intList 7   --  7 ->  2 as in list
