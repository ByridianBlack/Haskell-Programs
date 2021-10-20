
data Poly a = P [a] deriving (Show, Eq)


degree :: Poly a -> Int
degree (P[]) = 0
degree (P a) = num
    where num = length (extractList(P a)) - 1


scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale 0 _ = P []
scale r (P a) = new_list
    where
        old_list = extractList(P a)
        scaled_list = scaleHelp old_list r
        new_list = P scaled_list

-- scaleHelp [] r = []
-- scaleHelp (x:xs) r = x*r: scaleHelp xs r
scaleHelp xs r = map (*r) xs

addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly (P[]) (P (x:xs)) = P (x:xs)
addPoly (P[]) (P []) = P []
addPoly (P (x:xs)) (P []) = P (x:xs)
addPoly (P (x:xs)) (P (y:ys)) = added_list
    where
        combined_list = addedPolyHelper (x:xs) (y:ys)
        final         = trailer combined_list
        added_list    = P final 

addedPolyHelper (x:xs) [] = x:xs
addedPolyHelper [] (x:xs) = x:xs
addedPolyHelper [] [] = []
addedPolyHelper (x:xs) (y:ys) = x + y: addedPolyHelper xs ys
extractList (P (x:xs)) = x:xs
extractList (P []) = []


trailer :: (Eq a, Num a) => [a] -> [a]
trailer [] = []
trailer (x:xs) = trimmed_list
    where
        temp_list = x:xs
        reversed_list = reverse temp_list
        cut_list = cutter reversed_list
        trimmed_list = reverse cut_list
cutter [] = []
cutter (x:xs) = if x == 0 then cutter xs else x:xs


($$) :: (Num a, Eq a) => Poly a -> a -> a
P(x:[]) $$ r = x
P(x:xs) $$ r = data_point 
    where
        data_point = x + r * P(xs) $$ r

    

    

