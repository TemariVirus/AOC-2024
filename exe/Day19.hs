module Day19 where

import Common (split, startsWith)

matches :: [String] -> String -> [Int]
matches _ [] = [1]
matches towels pattern =
  sum (map matches' towels) : rest
  where
    rest = matches towels (tail pattern)
    matches' t = if pattern `startsWith` t then rest !! (length t - 1) else 0

part :: String -> [Int]
part input = case lines input of
  towels : _ : patterns -> map (head . matches (split ", " towels)) patterns
  _ -> error "Invalid input"

part1 :: String -> Int
part1 = length . filter (> 0) . part

part2 :: String -> Int
part2 = sum . part
