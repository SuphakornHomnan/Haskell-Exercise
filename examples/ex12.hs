data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

mapTree f Empty = Empty
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

--test case: mapTree (+3) (Node Empty 5 Empty)
-- ans: Node Empty 8 Empty
--test case: mapTree (\x-> x-3) (Node Empty 5 Empty)
--ans: Node Empty 2 Empty

-- what's the type? => mapTree:: (t->a) -> Tree t -> Tree a

foldTree f acc Empty = acc
foldTree f acc (Node l x r) = f x (foldTree f aux r)
  where
    aux = foldTree f acc l

-- test case: foldTree (+) 10 (Node Empty 4 Empty)
-- ans: 14
-- test case: foldTree (+) 10 (Node (Node Empty 2 Empty) 4 (Node Empty 7 Empty))
-- ans: 23

--what's the type?
-- foldTree:: (t1->t2->t2) -> t2 -> Tree t1 -> t2
--how many different folds can you come up with?
-- 6 inorder postorder preorder in foldl&foldr

-- write a function height that returns the heights of a given binary tree
-- height of the empty tree is 0
-- otherwise, take max of left child's height and right child's height, then add 1
findHeightTree Empty = 0
findHeightTree (Node l x r) = 1 + max (findHeightTree l) (findHeightTree r)

-- write a function isBST that takes a binary tree, and determine if it is a binary search tree
-- what's the type?
-- hint: a helper function helps!
isBST Empty = True
isBST (Node l x r)
  | ((l /= Empty) && (getValue l >= x)) || ((r /= Empty) && (getValue r < x)) = False
  | otherwise = isBST (l) && isBST (r)
  where
    getValue node = case node of
      Node _ a _ -> a

-- AVL Tree คือ algorithm ที่ใช้ทำให้Tree มีความbalanceกันทั้งสองฝั่ง โดยที่ฝั่งซ้ายกับขวาต้องมีdepthต่างกันไม่เกิน1
-- หากมีค่าต่างเกินจะมีการหมุนเพื่อให้ Tree มีการปรับbalanceใหม่