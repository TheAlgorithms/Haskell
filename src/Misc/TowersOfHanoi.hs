module Misc.TowersOfHanoi where

hanoi :: (Eq a, Num a) => a -> [Char] -> [Char] -> [Char] -> IO ()
hanoi 0 _ _ _ = return ()
hanoi n startPole intermediatePole endPole = do
    hanoi (n - 1) startPole endPole intermediatePole
    putStrLn ("Move from " ++ startPole ++ " to " ++ endPole)
    hanoi (n - 1) intermediatePole startPole endPole

main :: IO ()
main = do
    hanoi 3 "startPole" "intermediatePole" "endPole"
