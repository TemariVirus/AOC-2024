module Common where

import Data.Maybe (catMaybes, isJust)

splitOn :: String -> String -> [String]
splitOn delim = foldr (splitOn' delim) [""]

splitOn' :: String -> Char -> [String] -> [String]
splitOn' delim c (x : xs)
  | take (length delim) (c : x) == delim =
      "" : drop (length delim - 1) x : xs
  | otherwise = (c : x) : xs
splitOn' _ _ _ = error "splitOn': empty list"

isSorted :: (Ord a) => [a] -> Bool
isSorted arr = case arr of
  x : y : _ -> x <= y && isSorted (tail arr)
  _ -> True

windows :: Int -> [a] -> [[a]]
windows n arr
  | length arr < n = []
  | otherwise = take n arr : windows n (tail arr)

mapWhile :: (a -> Maybe b) -> [a] -> [b]
mapWhile f = catMaybes . takeWhile isJust . map f
