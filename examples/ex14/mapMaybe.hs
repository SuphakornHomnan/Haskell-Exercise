mapMaybe f x = case x of
  Nothing -> Nothing
  Just a -> Just $ f a