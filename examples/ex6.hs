filter_concat prev list = aux list []
    where 
        aux [] res = res
        aux (x:xs) res 
            | prev x = aux xs (res++x)
            | otherwise = aux xs res
-- what's the type of filter_concat?
-- filter_concat :: ([a]-> Bool) -> [[a]] -> [a]
-- can you avoid recursion in your definition?
-- (feel free to use functions from the Prelude)

filter_concat' prev list = concat $ filter prev list

take_while prev list = reverse(aux list [])
    where
        aux [] res = res 
        aux (x:xs) res  
            | prev x = aux xs (x:res)
            | otherwise = res
-- what's the type of take_while? 
-- take_while :: (a->Bool) -> [a] -> [a]

-- rewrite (\l -> length l < 3) without lambdas (\)
-- ((<3).length)
-- type : Foldable t => t a -> Bool
-- \f l -> l ++ map f l :: (a -> a) -> [a] -> [a]
   \f l -> ap (++) (map f) l 
    \f l -> (ap (++)) (map f) l
    \f l -> (ap (++)) ((map f) l)
    \f l -> ((ap (++)) .(map f) l
    \f  -> (ap (++)) .(map f) 
    \f  -> ((ap (++)) .)(map f) 
    \f  -> (((ap (++)) .).map) f 
     ((ap (++)) .).map  
     ((ap (++)) .).map  








