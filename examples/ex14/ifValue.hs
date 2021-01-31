class IfValue a where
  boolVal :: a -> Bool

instance IfValue Bool where
  boolVal False = False
  boolVal True = True

instance IfValue Char where
  boolVal _ = True

instance IfValue Double where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Float where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Integer where
  boolVal 0 = False
  boolVal _ = True

instance IfValue a => IfValue [a] where
  boolVal [] = False
  boolVal _ = True

instance IfValue a => IfValue (Maybe a) where
  boolVal Nothing = False
  boolVal _ = True