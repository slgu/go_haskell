--instance Functor Maybe where
--    fmap f (Just x) = Just (f x)
--    fmap f Nothing = Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
    
