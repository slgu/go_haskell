newtype Writer w a = Writer {runWriter :: (a,w)} deriving (Show)


instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

instance (Monoid w) => Applicative (Writer w) where
    (Writer (x, y)) <*> (Writer (x',y')) = Writer (x x', y')

instance (Monoid w) => Functor(Writer w) where
    fmap f (Writer (x,y)) = Writer (f x, y)


--add writer monad as log for functions

tell :: (Monoid m) => m -> Writer m Int
tell x = Writer (0,x)
gcdWithLog :: Int -> Int -> Writer [[Char]] Int
gcdWithLog a b
    | b == 0 = do
        tell ["a = " ++ show a ++ " b = 0"]
        return a
    | otherwise = do
        tell ["a = " ++ show a ++ " b = " ++ show b]
        gcdWithLog b (a `mod` b)

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList x) `mappend` (DiffList y) = DiffList (\xs -> x (y xs))

finalCountDown :: Int -> Writer (DiffList String) Int
finalCountDown 0 = do
    tell (toDiffList ["0"])

finalCountDown x = do
    finalCountDown (x - 1)
    tell (toDiffList [show x])


--really slow for array ++
finalCountDown2 :: Int -> Writer [String] Int
finalCountDown2 0 = do
    tell ["0"]

finalCountDown2 x = do
    finalCountDown2 (x - 1)
    tell [show x]
