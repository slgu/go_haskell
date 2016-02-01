insertAt x arr n = (take (n - 1) arr) ++ [x] ++ (drop (n - 1) arr)
insertAt' :: a -> [a] -> Int -> [a]
insertAt' x ys 1 = x:ys
insertAt' x (y:ys) n = y:insertAt' x ys (n - 1)
