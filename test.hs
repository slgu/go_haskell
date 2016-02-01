data TreeNode a = TreeNode {
    left  :: Maybe (TreeNode a),
    right :: Maybe (TreeNode a),
    value :: a
    } deriving Show


treeByLevels :: Maybe (TreeNode a) -> [a]


treeByLevels Nothing = []
treeByLevels a = concat (helper a)
    where
        helper Nothing = []
        helper (Just (TreeNode left right value)) =
            let rightRes = helper right
                leftRes = helper left
            in [value]: zipWithPad (\x y -> x ++ y) leftRes rightRes
			where
                zipWithPad :: (a -> a -> a) -> [a] -> [a] -> [a]
                zipWithPad f [] right_extra = right_extra
                zipWithPad f left_extra [] = left_extra
                zipWithPad f (x:xs) (y:ys) = (f x y):zipWithPad f xs ys
