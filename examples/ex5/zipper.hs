zipper x y = reverse (helperZip x y [])
  where
    helperZip [] _ answer = answer
    helperZip _ [] answer = answer
    helperZip (x : xs) (y : ys) answer = helperZip xs ys ((x, y) : answer)