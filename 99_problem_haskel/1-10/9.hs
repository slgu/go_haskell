pack [] = []
pack (xs@(x:ys)) = (takeWhile (==x) xs):pack(dropWhile (==x) xs)

pack' [] = []
pack' (x:xs) = let (first, rest) = span (==x) xs in (x:first) : pack rest
