main = do
  print $filter (isPalin.toBinary) (filter isPalin [1..1000000])
  print.sum $filter (isPalin.toBinary) (filter isPalin [1..1000000])

mirror :: Integer -> Integer
mirror = read.reverse.show

isPalin :: Integer -> Bool
isPalin a = (==) a $mirror a

toBinary :: Integer -> Integer
toBinary a = read.reverse.concat.(map show).toBin $a
  where toBin 0 = [0]
        toBin 1 = [1]
        toBin a = (mod a 2) : (toBin(quot a 2))