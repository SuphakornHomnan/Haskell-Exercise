data COp a = CVal Int a
    deriving (Show)

instance Functor COp where
  fmap f (CVal c v) = CVal (c+1) (f v)
