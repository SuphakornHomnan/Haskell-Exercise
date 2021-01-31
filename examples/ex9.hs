foldl' f acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

foldr' f acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

reverse' xs = foldl (\acc -> (: acc)) [] xs

reverse'' xs = foldr (\x acc -> acc ++ [x]) [] xs

--which version is more efficient, and why?
-- foldr is more efficient because foldr is tail recursive but foldl is just recursive
map' f [] = []
map' f xs = foldr (\y -> (:) (f y)) [] xs

filter' p = foldr (\x xs -> if p x then x : xs else xs) []