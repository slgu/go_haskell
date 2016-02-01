--error handle needed for get index
removeAt n arr = (arr !! (n - 1), [c | (i,c) <- (zip [1,2..] arr), i /= n])


--error detection solution
removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' 1 (x:xs) = (Just x, xs)
removeAt' k (x:xs) = let (a, r) = removeAt' (k - 1) xs in (a, x:r)
