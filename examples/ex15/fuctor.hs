-- My Fuctor
class FuctorAux f where 
    fmap :: (a->b) -> f a -> f b
instance FuctorAux ((->) r) where
  fmap f g = f.g
      