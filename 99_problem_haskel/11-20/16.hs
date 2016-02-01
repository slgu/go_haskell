dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery arr n = dropEveryHelper arr n n
    where
        dropEveryHelper (item:xs) x y
            | y == 1 = dropEveryHelper xs x x
            | otherwise = item:(dropEveryHelper xs x (y - 1))
        dropEveryHelper [] _ _ = []

dropEvery' :: [a] -> Int -> [a]
dropEvery' [] _ = []
dropEvery' list count = (take (count - 1) list) ++ dropEvery (drop count list) count


dropEvery'' :: [a] -> Int -> [a]
dropEvery'' xs n = [i | (i, c) <- (zip xs [1,2..]), (mod c n) /= 0]
