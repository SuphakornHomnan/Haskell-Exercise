mapList (function) [] = []
mapList (function) (x : xs) = function x : mapList (function) xs

mapList' function list = reverse (helperMap list [])
  where
    helperMap [] list = list
    helperMap (a : as) list = helperMap as (function a : list)