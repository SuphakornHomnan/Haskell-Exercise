-- write this function in point-free style:
contains1 x l = any (x<) l
--hint: any :: (a -> Bool) -> [a] -> Bool
-- \x l -> any (x<) l
-- \x  -> any (x<) 
-- \x  -> any (x)(<) 
-- \x  -> any (<)(x) 
-- \x  -> (any) ((<)(x)) 
-- \x  -> ((any) .(<))(x) 
-- (any) .(<) 

contains1' :: (Foldable t, Ord a) => a -> t a -> Bool
contains1' = (any) .(<)

-- write this function in point-free style
contains2:: (Foldable t, Ord a) => t a -> a -> Bool
contains2 = flip (any .(<))


-- write function len_comp that uses list comprehension to compute the length of the given list
len_comp list = sum [length [a] | a <- list]

-- rewrite
-- [(x,y) | x <- [2,3,5], y <- [1,2,4],
-- even $ x+y]
-- without using list comprehension
even_aux [] ys = []
even_aux (x : xs) ys = filter (\(x,y)-> even $ (x+y)) ((cross_prod x ys) ++ (even_aux xs ys) )
    where 
        cross_prod _ [] = []
        cross_prod x (y:ys) = (x,y) : (cross_prod x ys)