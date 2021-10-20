heads :: [a] -> Maybe a
heads[] = Nothing
heads (x:xs) = Just x
 

final [] = Nothing
final [x] = Just x
final (x:xs) = final xs

data Tree a = Tip | Bin (Tree a) a (Tree a)

sumTree Tip = 0
sumTree (Bin left root right) = root + sumTree(left) + sumTree(right)




