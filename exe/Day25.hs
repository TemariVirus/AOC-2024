module Day25 where

import Common (split)
import Data.List (transpose)

parseInput :: String -> ([[Int]], [[Int]])
parseInput input =
  let schematics = split "\n\n" input
      locks = filter (all (== '#') . head . lines) schematics
      keys = filter (all (== '#') . last . lines) schematics
      colHeights = map (length . filter (== '#')) . transpose . lines
   in (map colHeights locks, map colHeights keys)

part1 :: String -> Int
part1 input =
  let (locks, keys) = parseInput input
   in length
        [ Nothing
          | l <- locks,
            k <- keys,
            all ((< 8) . uncurry (+)) (zip l k)
        ]
