myButLast (x:y:z:xs) = myButLast (y:z:xs)
myButLast (x:y:xs) = x
