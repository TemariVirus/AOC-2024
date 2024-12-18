module Day18 where

import Common (binarySearchL, split)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..), rotateCW90)

data Node = Node
  { pos :: Vec2,
    g :: Int
  }
  deriving (Show, Eq)

instance Ord Node where
  compare (Node p1 g1) (Node p2 g2) = compare (g1, p1) (g2, p2)

parseInput :: String -> [Vec2]
parseInput input = map ((\[x, y] -> Vec2 (x, y)) . map read . split ",") $ lines input

gridSize :: Int
gridSize = 70

neighbors :: Set Vec2 -> Node -> [Node]
neighbors walls (Node pos g) =
  [ Node v (g + 1)
    | step <- take 4 $ iterate rotateCW90 (Vec2 (1, 0)),
      v@(Vec2 (x, y)) <- [pos + step],
      Set.notMember v walls && x >= 0 && y >= 0 && x <= gridSize && y <= gridSize
  ]

dijkstra :: Set Vec2 -> Vec2 -> Map Vec2 Int
dijkstra walls start =
  go
    (Map.singleton (state startNode) 0)
    (Set.singleton startNode)
  where
    state (Node p _) = p
    startNode = Node start 0
    go gs open =
      let current = fromMaybe (error "No path") (Set.lookupMin open)
          ns =
            filter
              ( \n -> case Map.lookup (state n) gs of
                  Nothing -> True
                  Just g' -> g n < g'
              )
              $ neighbors walls current
          ns' = Map.fromList $ map (\n -> (state n, g n)) ns
          gs' = ns' `Map.union` gs
          open' = Set.fromList ns `Set.union` Set.delete current open
       in if Set.null open then gs else go gs' open'

part1 :: String -> Int
part1 input =
  let walls = Set.fromList $ take 1024 $ parseInput input
   in dijkstra walls (Vec2 (0, 0)) Map.! Vec2 (gridSize, gridSize)

part2 :: String -> String
part2 input =
  let bytes = parseInput input
      reachable t =
        isJust $
          dijkstra
            (Set.fromList $ take (t + 1) bytes)
            (Vec2 (0, 0))
            Map.!? Vec2 (gridSize, gridSize)
      (Vec2 (x, y)) = bytes !! binarySearchL reachable (Seq.fromList [0 .. length bytes - 1])
   in show x ++ "," ++ show y
