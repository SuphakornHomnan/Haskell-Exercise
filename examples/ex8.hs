
-- what's the type of partition?
-- partition :: (a-> Bool) -> [a] -> ([a],[a])
-- what does partition do?
-- ทำการคัดแยกค่าในอาเรย์ ถ้าค่าในอาเรย์นั้นตรงตามเงื่อนไขก็จะนำไปไว้ในลิสต์ฝั่งซ้าย ถ้าค่าไม่ตรงตามเงื่อนไข
-- ก็จะนำค่าไปไว้ฝั่งด้านขวา
partition p [] = ([], [])
partition p (x:xs)            
    | p x       = (x:l, r)
    | otherwise = (l, x:r)
        where (l, r) = partition p xs

--rewrite filter using partition
--hint: see fst and snd functions
filter_part pred [] = []
filter_part pred list = fst $ partition pred list

    

--rewrite quicksort without using list comprehension
--hint: modify the version with where
--hint: use partition
qsort []      = []
qsort (hd:tl) =
    qsort l ++ [hd] ++ qsort r
  where l = fst $ partition (<hd) tl
        r = snd $ partition (<hd) tl

--look up type Ordering
-- compare :: a -> a -> Ordering
--what is it for?
-- เปรียบเทียบค่าด้วยเครื่องหมายต่างๆ เช่น มากกว่า น้อยกว่า เท่ากับ 
--how many constructors are there?
-- 3 constructors 
-- data Ordering = LT | EQ | GT 
--how many ways can we pattern-match an Ordering value?
-- 3 ways 

