--all :: (a -> Bool) -> [a] -> Bol

--all odd [2, 6, 1, 4, 9, 7]
--False
all' pred l = foldl (\acc x -> if pred x then acc else False) True l

-- write function elem that determines whether a given element is a member of a list
elem' pred = foldl (\acc x -> if pred == x then True else acc) False

-- type
-- elem' :: (Foldable t,Eq a) => a -> t a -> Bool
-- can you use fold? yes

-- analyze efficiency of both versions of map that use fold
-- foldr ดีกว่า เพราะ foldl ต้องเอาไป reverse อีก

--rewrite partition using fold
--you might need to write another function
--be sure to think about efficiency by choosing the appropriate kind of fold

partition' p = foldr (\x acc -> if p x then (x : fst (acc), snd (acc)) else (fst (acc), x : snd (acc))) ([], [])

foldl' f xs list = foldr (\x acc -> flip f acc x) xs list

foldr' f xs list = foldl (\acc x -> flip f acc x) xs list