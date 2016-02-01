data Tuple a b = Multiple a b | Single b deriving(Show)
encode_direct :: (Eq c) => (Num b) => [c] -> [Tuple b c]
encode_direct = foldr encode_helper []
    where encode_helper x [] = [Single x]
          encode_helper x xs@((Single y):ys)
            | x == y = (Multiple 2 x):ys
            | otherwise = (Single x):xs
          encode_helper x xs@((Multiple n y):ys)
            | x == y = (Multiple (n + 1) x):ys
            | otherwise = (Single x):xs
