revList x = case x of
  [] -> []
  (x : xs) -> revList xs ++ [x]