data Tree a = Empty | Branch a (Tree a) (Tree a) deriving(Show, Eq)

leaf x = Branch x Empty Empty

istree x = case x of
    Empty -> True
    Branch _ _ _ -> True


cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n | ((mod n 2) /= 0) = let
    sub_n = (n - 1) `div` 2
    sub_ans = cbalTree sub_n
    in concat [map x sub_ans|x <- [Branch 'x' x| x <- sub_ans]]

cbalTree n = let
    sub_n = (n - 1) `div` 2
    sub_ans_v1 = cbalTree sub_n
    sub_ans_v2 = cbalTree (sub_n + 1)
    res_v1 = concat [map x sub_ans_v2|x <- [Branch 'x' x| x <- sub_ans_v1]]
    res_v2 = concat [map x sub_ans_v1|x <- [Branch 'x' x| x <- sub_ans_v2]]
    in res_v1 ++ res_v2

synmetric Empty = True
synmetric (Branch char Empty Empty) = True
synmetric node = let
    in expression
