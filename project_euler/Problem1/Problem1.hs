solList = filter (\n -> (rem n 5 == 0) || (rem n 3 == 0)) [1..1000]

main = do
    print solList