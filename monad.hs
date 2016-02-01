type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole

landLeft n (left, right)
    | abs((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs(left - right - n) < 4 = Just (left, right + n)
    | otherwise = Nothing

x -: f = f x --(easy to simulate this problem)

main = test_do
test_monad = do
    let res = Just (0,0) >>= landLeft 4 >>= landRight 1 >>= landLeft 2
    print res

--very nice usage of monad (usage of Maybe and deal with error use >>= Nothing judge)



foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
--do for monad
test_do = do
    print foo

--the last statement of do is like the return value

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing --
    second <- landRight 2 first
    landLeft 1 second

--If we have a Maybe String and we bind it with <- to a variable,
--that variable will be a String

--a value like 5 is a deterministic value,
--however a value like [3,8,9] contains serveral values

--list monad

--ghci> [3,4,5] >>= \x -> [x,-x]
--[3,-3,4,-4,5,-5]

--analyze [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

--In this case, it's making the smallest possible list that still presents (n,ch) as the
--result and features as little non-determinism as possible
-- (it is much the same as list comprehension)

--an explicit usage of guard [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

--Q: what is non-determinism means
