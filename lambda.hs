main = print (mp 3)
mp x = map (\a  -> a * 3) [1..x]

summ xs = foldl (\acc x -> acc + x) 0 xs
--easier version
--need function type definition
--otherwise ambiguous
summ_v2 :: (Num a) => [a] -> a
summ_v2 = foldl (\acc x -> acc + x) 0
report_summ :: (Num a) => [a] -> [a]
report_summ = scanl1 (\acc x -> acc + x)

-- $ has lowest precedence and is right-associative
apply_three :: (Num a) => (a -> a) -> (a -> a) -> (a -> a) -> a -> a
apply_three f g z x = f $ g $ z x
-- apply_three (+3) (*4) (5-) 0
