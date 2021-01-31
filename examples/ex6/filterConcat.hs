filterConcat prev list = helper list []
  where
    helper [] result = result
    helper (x : xs) result
      | prev x = helper xs (result ++ x)
      | otherwise = helper xs result

filterConcat' prev list = concat $ filter prev list