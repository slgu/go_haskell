data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float| Rectangle Point Point deriving(Show)

--map (Circle 10 20) [4,5,6,7]
surface :: Shape -> Float

surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
nudge (Circle (Point x y) r) a b  = Circle (Point (x + a) (y + b)) r


--a person data structure
data Person = Person String String Int Float String String deriving (Show)
name (Person x _ _ _ _ _) = x
age (Person _ _ x _ _ _) = x
main = do
        let guy = Person "slgu" "columbia university" 22 1.84 "programmer" "fuck"
        print guy
        print $ age guy
        print $ name guy
        let guy2 = Person_v2 "slgu" "columbia university" 22 1.84 "programmer" "fuck"
        print $ age_v2 guy2


data Person_v2 = Person_v2 {
    name_v2 :: String,
    addr :: String,
    age_v2 :: Int,
    height :: Float,
    position :: String,
    intro :: String
} deriving (Show)

--Just 10 == Just 20 True
--Just 10 == Nothing False
