listZipper x = case x of
  ([], _) -> []
  (_, []) -> []
  ((x : xs), (y : ys)) -> (x, y) : listZipper (xs, ys)