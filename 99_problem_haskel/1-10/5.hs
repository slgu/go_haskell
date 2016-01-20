myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


myReverse' :: [a] -> [a]
myReverse'          =  foldl (flip (:)) []
