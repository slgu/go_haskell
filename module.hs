import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Geometry

flatten :: (Num a) => [a] -> [a] -> [a]
flatten x y = L.intercalate [] [x, y]
main = do print $ L.intersperse '.' "MONKEY"
          print $ flatten [1,2,3] [4,5,6]
          print $ L.concat [[1,3,4],[1,2,3]]
          print phoneBook
          print a
          print $ find_key "betty" phoneBook
          let inserted_mp = Map.insert "ads" "sadasd" mp
          print inserted_mp
          print $ Geometry.cube_area 3.4
phoneBook = [("betty","555-2938")]
a = 3
b = 2 * a
--("wendy","939-8282"),("penny","853-2492")]

--self implementation
find_key :: (Eq k) => k -> [(k,v)] -> Maybe v
find_key key [] = Nothing
find_key key ((k,v):xs) = if key == k then Just v
                                else find_key key xs

mp = Map.fromList phoneBook
