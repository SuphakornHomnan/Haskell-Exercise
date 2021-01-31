join x list = case x of
  [] -> list
  (x : xs) -> x : join xs list