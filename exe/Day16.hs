module Day16 where

import Common (dijkstra)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..), rotateCCW90, rotateCW90)

type Node = (Vec2, Vec2)

east :: Vec2
east = Vec2 (1, 0)

west :: Vec2
west = Vec2 (-1, 0)

south :: Vec2
south = Vec2 (0, 1)

parseInput :: String -> (Set Vec2, Vec2, Vec2)
parseInput input =
  let chars =
        [ (Vec2 (x, y), c)
          | (y, line) <- zip [0 ..] $ lines input,
            (x, c) <- zip [0 ..] line
        ]
      find' c = fst $ fromJust $ find ((== c) . snd) chars
      start = find' 'S'
      end = find' 'E'
      spaces = Set.fromList $ map fst $ filter ((/= '#') . snd) chars
   in (spaces, start, end)

neighbors :: Set Vec2 -> (Int, Node) -> [(Int, Node)]
neighbors spaces (g, (pos', dir)) =
  filter
    (\(_, (p, _)) -> p `Set.member` spaces)
    [ (g + 1, (pos' + dir, dir)),
      (g + 1000, (pos', rotateCW90 dir)),
      (g + 1000, (pos', rotateCCW90 dir))
    ]

tileCost :: Map (Vec2, Vec2) Int -> Vec2 -> Int
tileCost gs tile = minimum $ map snd $ Map.toList $ Map.filterWithKey (\(pos, _) _ -> pos == tile) gs

part1 :: String -> Int
part1 input =
  let (spaces, start, end) = parseInput input
      gs = dijkstra (neighbors spaces) (start, east)
   in tileCost gs end

part2 :: String -> Int
part2 input =
  let (spaces, start, end) = parseInput input
      dijkstra' = dijkstra (neighbors spaces)
      gs = dijkstra' (start, east)
      gs' = Map.unionWith min (dijkstra' (end, west)) (dijkstra' (end, south))
      cost = tileCost gs end
      dirs = take 4 $ iterate rotateCW90 east
   in length
        $ filter
          ( \pos ->
              or
                [ fromJust (Map.lookup (pos, d) gs')
                    + fromJust (Map.lookup (pos, negate d) gs)
                    == cost
                  | d <- dirs
                ]
          )
        $ Set.toList spaces
