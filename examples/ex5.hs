fib n = fib_aux 0 0 1
  where
    fib_aux i res res'
      | i == n    = res
      | otherwise =
          fib_aux (i+1) res' (res+res')

rev ([]) = []
rev ((x:xs)) = rev (xs) ++ [x]

len [] = 0
len (n:ns) = 1 + len ns

rev' l = rev_aux l []
    where
        rev_aux [] ans = ans
        rev_aux (a:as) ans = rev_aux as (a:ans)

-- type of list_map 
-- list_map :: (t -> a) -> [t] -> [a]

list_map (func) [] = []
list_map (func) (x:xs) = func x : list_map (func) xs   
-- can you use tail recursion?
-- yes 
list_map' func l = rev' (ext l [])
    where
        ext [] l = l
        ext (a:as) l = ext as (func a : l) 
-- เขียน 3 test case list_map'
-- list_map' (\x->x-5) [89,60,38,92]
-- [84,55,33,87]
-- list_map' (\x->x/8) [89,60,38,92]
-- [11.125,7.5,4.75,11.5]
-- list_map' rev' ["nice","mikekiri"]
-- ["ecin","irikekim"]


zipper x y = rev' (aux x y [])
    where
        aux [] _ ans = ans
        aux _ [] ans = ans
        aux (x:xs) (y:ys) ans = aux xs ys ((x,y):ans)