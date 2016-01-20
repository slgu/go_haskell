myLength :: (Num a) => [a] -> a
myLength = foldl (\n _ -> n + 1) 0
