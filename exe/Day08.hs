module Day08 where

import Common (pairs)
import Data.List (groupBy, sortOn)
import Data.Set qualified as Set
import Vec2 (Vec2 (..))
import Vec2 qualified

parseInput :: String -> (Int, Int, [(Vec2, Char)])
parseInput input =
  let width = length $ head $ lines input
      height = length $ lines input
      antennas =
        [ (Vec2 (x, y), c)
          | (y, line) <- zip [0 ..] (lines input),
            (x, c) <- zip [0 ..] line,
            c /= '.'
        ]
   in (width, height, antennas)

groupAntennas :: [(Vec2, Char)] -> [[Vec2]]
groupAntennas = map (map fst) . groupBy (\a b -> snd a == snd b) . sortOn snd

antennaPairs :: [(Vec2, Char)] -> [(Vec2, Vec2)]
antennaPairs antennas = [x | xs <- map pairs $ groupAntennas antennas, x <- xs]

rayMarch :: Int -> Int -> Vec2 -> Vec2 -> [Vec2]
rayMarch w h start step =
  takeWhile
    (\(Vec2 (xn, yn)) -> xn >= 0 && xn < w && yn >= 0 && yn < h)
    [ start + Vec2.apply (* i) step
      | i <- [0 ..]
    ]

part :: ([Vec2] -> [Vec2]) -> String -> Int
part rayFilter input =
  let (width, height, antennas) = parseInput input
      ps = [p | (a, b) <- antennaPairs antennas, p <- [(a, b), (b, a)]]
      rayMarch' (a, b) = rayFilter $ rayMarch width height a (a - b)
   in (Set.size . Set.fromList)
        [ v
          | vs <- map rayMarch' ps,
            v <- vs
        ]

part1 :: String -> Int
part1 = part (take 1 . drop 1)

part2 :: String -> Int
part2 = part id
