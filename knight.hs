import Control.Monad
type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')


canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` result
    where
        f x = x:(moveKnight x)
        result = [start] >>= f >>= f >>= f


main = test_move_knight

test_move_knight = do
    print $ moveKnight (1,1)
    print $ canReachIn3 (1,1) (3,2)
    print $ canReachIn3 (3,2) (5,3)
    print $ canReachIn3 (1,1) (9,5)


--what does it truly mean by non-deterministic
--You're thinking "non-deterministic computation" as in "a program which doesn't fully determine its output". This kind of non-
--determinism is common when using multiple parallel threads of execution; there are
--many possible outputs, and which one you get is arbitrarily determined by the precise order in which things happen
--at runtime.
