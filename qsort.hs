--recursive call
qsort [] = []
qsort (x:xs) =
    let small_ones = qsort ([a | a <- xs, a <= x])
        big_ones = qsort ([a | a <- xs, a > x])
    in small_ones ++ [x] ++ big_ones


qsort_v2 [] = []
qsort_v2 [x] = [x]
qsort_v2 (x:xs) =
    small_ones ++ [x] ++ big_ones
    where
        small_ones = qsort_v2 ([a | a <- xs, a <= x])
        big_ones = qsort_v2 ([a | a <- xs, a > x])
