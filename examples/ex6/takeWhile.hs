takeWhile prev list = reverse (helper list [])
  where
    helper [] res = res
    helper (x : xs) res
      | prev x = helper xs (x : res)
      | otherwise = res