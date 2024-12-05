module Common where

splitOn :: String -> String -> [String]
splitOn delim = foldr (splitOn' delim) [""]

splitOn' :: String -> Char -> [String] -> [String]
splitOn' delim c (x : xs)
  | take (length delim) (c : x) == reverse delim =
      "" : drop (length delim - 1) x : xs
  | otherwise = (c : x) : xs
splitOn' _ _ _ = error "splitOn': empty list"
