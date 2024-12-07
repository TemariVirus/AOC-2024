module Day04 where

import Data.Map (Map)
import Data.Map qualified as Map
import Vec2 (Vec2 (..), orthogonal)

type WordSearch = Map Vec2 Char

parseInput :: String -> (Int, Int, WordSearch)
parseInput input =
  let width = length $ head $ lines input
      height = length $ lines input
      coordMap =
        Map.fromList
          [ (Vec2 (x, y), c)
            | (y, line) <- zip [0 ..] (lines input),
              (x, c) <- zip [0 ..] line
          ]
   in (width, height, coordMap)

directions :: [Vec2]
directions =
  [ Vec2 (x, y)
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      x /= 0 || y /= 0
  ]

searchLine :: String -> WordSearch -> Vec2 -> Vec2 -> Bool
searchLine [] _ _ _ = True
searchLine (x : xs) coordMap d p = case Map.lookup p coordMap of
  Just c -> c == x && searchLine xs coordMap d (p + d)
  Nothing -> False

part1 :: String -> Int
part1 input =
  let (width, height, coordMap) = parseInput input
   in length $
        filter
          id
          [ searchLine "XMAS" coordMap d (Vec2 (x, y))
            | x <- [0 .. width - 1],
              y <- [0 .. height - 1],
              d <- directions
          ]

searchXmas :: WordSearch -> Vec2 -> Bool
searchXmas coordMap p
  | Map.lookup p coordMap /= Just 'A' = False
  | length ms /= 2 = False
  | length ss /= 2 = False
  | (not . orthogonal) (head ms - last ms) = False
  | (not . orthogonal) (head ss - last ss) = False
  | otherwise = True
  where
    cornerCoords = [p + Vec2 (dx, dy) | dx <- [-1, 1], dy <- [-1, 1]]
    corners = map (\x -> (x, Map.lookup x coordMap)) cornerCoords
    ms = [x | (x, Just c) <- corners, c == 'M']
    ss = [x | (x, Just c) <- corners, c == 'S']

part2 :: String -> Int
part2 input =
  let (width, height, coordMap) = parseInput input
   in length $
        filter
          id
          [ searchXmas coordMap (Vec2 (x, y))
            | x <- [1 .. width - 2],
              y <- [1 .. height - 2]
          ]
