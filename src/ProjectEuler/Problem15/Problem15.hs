main = do
  print $ choose 40 20

choose :: Int -> Int -> Integer 
choose a b = pascal (a+1) (b+1)

pascal :: Int -> Int -> Integer
pascal a b = triangle !! a !! b
  where
    triangle = [[pascal' a b | b <- [0..]] | a <- [0..]]
    pascal' a b | a == 1 && b == 1  = 1
                | a <= 0 || b <= 0  = 0
                | otherwise         = triangle !! (a-1) !! (b-1) + triangle !! (a-1) !! b