palinedrome :: Eq a => [a] -> Bool
palinedrome xs = (xs == reverse xs)

main :: IO ()
main = do
  print (palinedrome "racecar")