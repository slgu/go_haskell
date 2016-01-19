rpn :: (Num a) => String -> a
rpn expression = head (foldl st_func [] (words expression))
    where st_func stack item
            | item == "-" = let a1:a2:_ = take 2 stack in (a2 - a1):(drop 2 stack)
            | item == "+" = let a1:a2:_ = take 2 stack in (a1 + a2):(drop 2 stack)
            | item == "*" = let a1:a2:_ = take 2 stack in (a1 * a2):(drop 2 stack)
            | otherwise = (fromIntegral (read item::Integer)):stack

main = print $ rpn_with_pm "10 4 3 + 2 * -"



rpn_with_pm expression = head (foldl st_func [] (words expression))
    where
        st_func (x:y:ys) "*" = (x * y):ys
        st_func (x:y:ys) "+" = (x + y):ys
        st_func (x:y:ys) "-" = (y - x):ys
        st_func stack num = (read num::Int):stack
