slice arr j k = [c |(c, i) <- (zip arr [1,2..]), i >= j && i <= k]
slice' ::[b] -> Int -> Int -> [b]
slice' xs i k = let
    chop = snd $ splitAt i xs
    res = fst $ splitAt (k - i) chop
    in res
