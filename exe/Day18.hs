module Day18 where

import Common (binarySearchL, dijkstra, split)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..), rotateCW90)

gridSize :: Int
gridSize = 70

parseInput :: String -> [Vec2]
parseInput input = map ((\[x, y] -> Vec2 (x, y)) . map read . split ",") $ lines input

neighbors :: Set Vec2 -> (Int, Vec2) -> [(Int, Vec2)]
neighbors walls (g, pos) =
  [ (g + 1, v)
    | step <- take 4 $ iterate rotateCW90 (Vec2 (1, 0)),
      v@(Vec2 (x, y)) <- [pos + step],
      Set.notMember v walls && x >= 0 && y >= 0 && x <= gridSize && y <= gridSize
  ]

part1 :: String -> Int
part1 input =
  let walls = Set.fromList $ take 1024 $ parseInput input
   in dijkstra (neighbors walls) (Vec2 (0, 0)) Map.! Vec2 (gridSize, gridSize)

part2 :: String -> String
part2 input =
  let bytes = parseInput input
      reachable t =
        isJust $
          dijkstra
            (neighbors $ Set.fromList $ take (t + 1) bytes)
            (Vec2 (0, 0))
            Map.!? Vec2 (gridSize, gridSize)
      (Vec2 (x, y)) = bytes !! binarySearchL reachable (Seq.fromList [0 .. length bytes - 1])
   in show x ++ "," ++ show y
