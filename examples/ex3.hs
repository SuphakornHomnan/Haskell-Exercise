
-- how long does join take?
-- Ans O(n)  
-- how long does your rev take to compute the reverse of a list
-- Ans O(n) เพราะว่า join กับ rev ถูกเรียกใช้ไปตามๆกัน สมมติถ้า len = 2 ก็จะทำการเรียก rev ไป 2 step และในการเรียกrevก็จะมีการเรียกjoinไปด้วย 
-- ตามนี้ rev(2) = join ( rev(1) , [x]) = join ( join (rev[],[y]), [x]) 
-- are you satisfied with the running time?
-- satisfied 

-- please take care of improper inputs
-- what's the type of your fib? 
-- Ans 
-- fib :: (Num a, Num p, Ord a) => a -> p
fib n 
    | n== 0 = 0
    | n== 1 = 1
    | n> 1  = fib (n-1) + fib (n-2) 
    | otherwise = error "Please enter only positive number!!!!"

-- how long does your fib take to compute fib n?
-- Ans O(2^n)
-- are you satisfied with the running time?
-- ไม่ดีพอ ใช้ for loop ก็จะได้ O(n)