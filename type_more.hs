--Easy enum
data Day = Monday | Tuesday | Wednesday | Thursday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

--typedef
type PhoneBook = [(String, String)]


--parameterized typedef
type AssocList k v = [(k,v)]

--data List a = Empty | Cons {list_head::a, list_tail :: List a} deriving (Show, Read, Eq, Ord)
data List1 a = Empty | a :-: (List1 a) deriving (Show, Read, Eq, Ord)

(.++) :: List1 a -> List1 a -> List1 a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

--a Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read)

single_node_tree :: a -> Tree a
single_node_tree x = Node x EmptyTree EmptyTree
tree_insert :: (Ord a) => (Tree a) -> a -> (Tree a)
tree_insert EmptyTree item = single_node_tree item
tree_insert (Node x y z) item =
    if item < x then Node x (tree_insert y item) z
        else Node x y (tree_insert z item)

tree_insert_v2 :: (Ord a) => (Tree a) -> a -> (Tree a)

tree_insert_v2 EmptyTree item = single_node_tree item
tree_insert_v2 (Node x y z) item
    | item < x = Node x (tree_insert_v2 y item) z
    | item > x = Node x y (tree_insert_v2 z item)
    | otherwise = Node x y z


tree_elem :: (Ord a) => a -> Tree a -> Bool
tree_elem item EmptyTree = False
tree_elem x (Node y left right)
    | x == y = True
    | x < y = tree_elem x left
    | otherwise = tree_elem x right

--define equal instance for class Eq
instance (Eq a) => (Eq (Tree a)) where
    EmptyTree == EmptyTree = True
    Node x1 y1 z1 == Node x2 y2 z2 = ((x1 == x2) && (y1 == y2) && (z1 == z2))
    _ == _ = False

class YesNo a where
    yesno :: a -> Bool

instance (YesNo (Tree a)) where
    yesno EmptyTree = False
    yesno (Node x1 y1 z1) = True

main = do
        let nums = [6,7,1,2,4,3,5]
        let tree_from_nums = foldl tree_insert_v2 EmptyTree nums
        print tree_from_nums
