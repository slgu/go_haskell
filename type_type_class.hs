
elem_in :: (Eq a) => a -> [a] -> Bool
elem_in x xs = foldl (\acc y -> if x == y then True else acc) False xs 
