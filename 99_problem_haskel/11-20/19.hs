rotate arr n = let
    len = length arr
    rotate_len = if n > 0 then n else len + n
    res_arr = (drop rotate_len arr) ++ (take rotate_len arr)
    in res_arr


--conditional pattern match (with guard expression)
rotate' [] _ = []
rotate' l 0 = l
rotate' (x:xs) n | n > 0 = rotate' (xs ++ [x]) (n - 1)
rotate' l n | n < 0 = rotate' l (length l + n)

rotate'' xs n = take len . drop (mod n len) . cycle $ xs
    where len = length xs
