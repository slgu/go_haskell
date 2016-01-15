#some note for haskell
lazy calculation:
    haskell won't try to evaluate the infinite list because it is lazy
    ghci> take 10 [13,26..]

The [Char] type is synonymous with String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

pattern matching:
    search function call according this pattern matching
    let <bindings> in <expression>.


but the thing is that the ++ function is much more expensive than :, so we usually use right folds when we're building up new lists from a list.
