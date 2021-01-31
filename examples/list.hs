zipper :: ([a], [b]) -> [(a, b)]
zipper ([],_) = []
zipper (_,[]) = []
zipper ((x:xs),(y:ys)) = (x,y) : zipper (xs,ys)
-- เอาตัว head ของทั้ง 2 list มาzipในวงเล็บ แล้วนำมาต่อกับ list ของหางที่ทำการzipper มีการทำrecusive ไปจนกว่าจะถึง base case
-- Ex [1,2] [3,4] => (1,3) : zipper ([2],[4]) => (1,3) : (2,4) : zipper ([],[]) => (1,3) : (2:4) : []
join :: ([a],[a]) -> [a]
join ([],ys) = ys
join ((x:xs),ys) = x : join (xs,ys)

rev :: ([a]) -> [a]
rev ([]) = []
rev ((x:xs)) = join (rev (xs), [x])
-- เอา listตัวหางมาใส่ใน rev แล้วนำมา join กับ list ของตัวหัว ทำแบบนี้ไปเรื่อยจนถึง base case  
-- Ex [1,2,3] => rev [2,3] ++ [1] => rev [3] ++ [2] ++ [1] => [] ++ [3] ++ [2] ++ [1] 

