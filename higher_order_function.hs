main = do x <- readLn
          y <- readLn
          print (find_largest_divide x y)

multi_three x y z = x * y * z
multi_one = multi_three 1 3
divide_by_ten = (/10)
apply_twice f x = f (f x)
--section an infix function
--(+) max (++)
--apply_twice (3:) [1]
zipwith _ _ [] = []
zipwith _ [] _ = []
zipwith f (x:xs) (y:ys) = f x y : zipwith f xs ys

flip f = g
    where g x y = f y x

mp x = map (+x) [1,2,3]

find_largest_divide x y = head (filter p [x,x-1..])
    where p k = (k `mod` y) == 0

my_chain :: (Integral a) => a -> [a]
my_chain 1 = [1]
my_chain n
    | even n = n:my_chain (n `div` 2)
    | odd n = n:my_chain (n * 3 + 1)

find_large_chain x y = filter p [1..x]
    where p k = length (my_chain k) >= y
