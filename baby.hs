

double_me x = x + x
double_small_number x = if x > 100 then x else x * 2
-- :t 'a'
--let triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2]

--sayme :: (Integral a) => a -> String
sayme 1 = "one"
sayme 2 = "two"
sayme x = "not 1 or 2"

add_vec :: (Num a) => (a, a) -> (a, a) -> (a, a)
add_vec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell [] = "the list is empty"
tell (x:[]) = "the list has element: " ++ show x
tell (x:y:[]) = "the list has element: " ++ show x ++ " and " ++ show y

tell (x:xs) = "the list has elemet" ++ show x ++ " " ++ tell xs

capital string@(x:xs) = "the first letter of " ++ string ++ " is " ++ [x]

fib x
    | x == 1 = 1
    | x == 2 = 1
    | otherwise = fib (x - 1) + fib (x - 2)


main_sqrt x = [ y | y <- [1..x], y * y == x]


sqrt_seq x = [let map_sqrt xs = [main_sqrt x | x <- xs] in map_sqrt [1..x]]
