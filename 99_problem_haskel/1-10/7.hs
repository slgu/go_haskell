data NestedList a = Elem a | List [NestedList a]

--flatten :: (NestedList a) => a -> [b]
flatten (Elem i) = [i]
flatten (List xs) = foldl (\arr item -> arr ++ (flatten item)) [] xs
