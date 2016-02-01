encode [] = []
encode (x:xs) = let
    extra_encode = encode xs
    in if length extra_encode == 0 || snd (head extra_encode) /= x then (1,x):extra_encode
        else (fst (head extra_encode) + 1, snd (head extra_encode)):(tail extra_encode)


data Tuple a b = Multiple a b | Single b deriving(Show)
encodeModified xs = map helper (encode xs)
    where
        helper (1,x) = Single x
        helper (n,x) = Multiple n x
