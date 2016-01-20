--straight forward
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)
