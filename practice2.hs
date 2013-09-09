m i
 | i>10 = 10
 | otherwise = i

-- elem' y ys = foldl (\ b a -> if a == y then True else b) False ys


-- (foldr (\ acc a -> trace (show acc) a) [a]) $ (map (\a -> [a])) [12,2,4,52,2,65,4,2]

-- (foldr (\ acc a -> acc:(acc:a)) []) $ (map (\a -> [a])) [12,2,4,52,2,65,4,2]

f acc [] = [acc]
f acc (x:xs) = (x:xs) ++ acc