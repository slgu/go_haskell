import Data.List
compress :: (Eq a) => [a] -> [a]
compress = foldl (\arr item -> if length arr /= 0 && (last arr) == item then arr else arr ++ [item]) []

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group



compress'' (x:ys@(y:_))
    | x == y    = compress'' ys
    | otherwise = x : compress'' ys
compress'' ys = ys


compress''' (x:xs) = x : (compress''' $ dropWhile (== x) xs)
