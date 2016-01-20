import Data.List
encode [] = []
encode (x:xs) = let
    extra_encode = encode xs
    in if length extra_encode == 0 || snd (head extra_encode) /= x then (1,x):extra_encode
        else (fst (head extra_encode) + 1, snd (head extra_encode)):(tail extra_encode)


encode' xs = map (\x -> (length x, head x)) (group xs)


encode'' :: Eq a => [a] -> [(Int,a)]
final (Just a@(i,q)) = [a]
final Nothing = []
f x r (Just a@(i,q)) | x == q = r (Just (i+1,q))
                     | otherwise = a : r (Just (1, x))
f x r Nothing = r (Just (1, x))
encode'' xs = foldr f final xs Nothing


--explanation
--misleading foldr takes just three elements
--be careful of function type
--f x r will returns a function that clojures 'x'.
--so after foldr, foldr f final xs will return a function
--final x1 (final x2(final x3....))
--last accept a nothing and deal with it from bottom to top.

inc :: Int -> (Int -> Int)
inc v = \x -> x + v
main = print $ inc 12 123
