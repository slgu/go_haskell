import qualified Data.Foldable as F
newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)


newtype Pair b a = Pair {getPair :: (a, b)} deriving (Show)
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

--this apply to pair means fmap (a -> b) (Pair c a) -> (Pair c b)
--so we can not change the second value . (which is related to c)

newtype PairSame a = PairSame {getSamePair :: (a, a)} deriving (Show)

instance Functor PairSame where
    fmap f (PairSame (x, y)) = PairSame (f x, f y)

--in this way fmap (a -> b) (PairSame a) -> PairSame b

--result: which can be functors (accept one type and generate another type(itself)).

--why newtype is faster than data

--haskell is lazy by default

data CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String

helloMe (CoolBool _) = "hello"


--helloMe (CoolBool True)
--helloMe undefined So in order to see if the value given to our function conforms to the (CoolBool _) pattern,
--Haskell has to evaluate the value just enough to see which value constructor was used when we made the value.
-- And when we try to evaluate an undefined value, even a little, an exception is thrown.


main = test_fold_tree


newtype WrapInt = WrapInt Int deriving (Show)
instance Monoid WrapInt where
    mempty = WrapInt 1
    (WrapInt x) `mappend` (WrapInt y) = WrapInt (x * y)

--monoid is something that support mempty mappend (which means identity and associative)



--foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r


newtype Any = Any {getAny :: Bool} deriving (Show)
instance Monoid Any where
    mempty = Any False
    (Any x) `mappend` (Any y) = Any (x || y)

test_fold_tree = do
    let testTree = Node 5
                    (Node 3
                        (Node 1 Empty Empty)
                        (Node 6 Empty Empty)
                    )
                    (Node 9
                        (Node 8 Empty Empty)
                        (Node 10 Empty Empty)
                    )
    print $ F.foldl (+) 0 testTree
    print $ getAny $ F.foldMap (\x -> Any $ x == 2) testTree
    print $ F.foldMap (\x -> [x]) testTree
