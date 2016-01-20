myLast [x] = x
myLast (x:xs) = myLast xs


--another solution
myLast = foldr1 (const id)
