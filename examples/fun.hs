-- type fac --
fac :: (Num p, Ord p) => p -> p
fac 0 = 1 
fac n = n * (n-1)
-- safe fac should have id condition for checking n that n is non-negative number if 
-- not give throw error exception 
zipper ([],_) = []
zipper (_,[]) = []
zipper ((x:xs),(y:ys)) = (x,y) : zipper (xs,ys)


-- zipper' :: [a] -> [b] -> [(a, b)]  type of zipper'
-- zipper' [] :: [b] -> [(a, b)]  type of zipper'
zipper' = \x y -> zipper (x,y)




