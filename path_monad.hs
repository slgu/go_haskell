--look at log monad
import Data.Monoid
import Control.Monad
data Path z = Path (Sum Int) String z deriving (Show)

instance Functor Path where
    fmap f (Path x y z) = Path x y (f z)
instance Applicative Path where
    pure = Path (Sum 0) ""
    (Path x y z) <*> (Path x' y' k) = Path x y (z k)

--when ever you defines a monad you must register it for Functor
-- and Applicative

instance Monad Path where
    return z = Path mempty mempty z
    (Path x y z) >>= f =
        let (Path x' y' z') = f z
        in (Path (x `mappend` x') (y `mappend` y') z')
        --a very strong type system (mistake z' write for z)

--calculate Path
drive :: String -> (Path String)
drive "" = Path 0 "" "england"
drive "england" = Path 12 "england" "usa"
drive "usa" = Path 15 "usa" "china"
drive _ = Path 142857 "nima" "nima"



main = do
    print test_do_notation_monad

test_monad = do
    let res = (return "") >>= drive >>= drive >>= drive
    print res


doPath :: String -> Path String

doPath x = Path 1 "test" x
test_do_notation_monad = do
    a <- doPath "hha"
    b <- doPath "hha"
    return (a ++ b)
