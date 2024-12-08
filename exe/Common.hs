module Common where

import Data.Maybe (catMaybes, isJust)

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = False

splitOnce :: (Eq a) => [a] -> [a] -> ([a], [a])
splitOnce delim = go []
  where
    go acc [] = (reverse acc, [])
    go acc str@(x : xs)
      | str `startsWith` delim = (reverse acc, drop (length delim) str)
      | otherwise = go (x : acc) xs

split :: (Eq a) => [a] -> [a] -> [[a]]
split _ [] = []
split delim str = x : split delim xs where (x, xs) = splitOnce delim str

isSorted :: (Ord a) => [a] -> Bool
isSorted arr@(x : y : _) = x <= y && isSorted (tail arr)
isSorted _ = True

pairs :: [a] -> [(a, a)]
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs
pairs _ = []

windows :: Int -> [a] -> [[a]]
windows n arr
  | length arr < n = []
  | otherwise = take n arr : windows n (tail arr)

mapWhile :: (a -> Maybe b) -> [a] -> [b]
mapWhile f = catMaybes . takeWhile isJust . map f
