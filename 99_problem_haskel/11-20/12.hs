data Tuple a b = Multiple a b | Single b deriving(Show)
encode_accumulate arr (Multiple a b) = arr ++ (take a (repeat b))
encode_accumulate arr (Single a) = arr ++ [a]
decodeModified tuples = foldl encode_accumulate [] tuples
