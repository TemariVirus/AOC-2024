module Day16 where

import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..), rotateCCW90, rotateCW90)

data Node = Node
  { pos :: Vec2,
    dir :: Vec2,
    g :: Int
  }
  deriving (Show, Eq)

instance Ord Node where
  compare (Node p1 d1 g1) (Node p2 d2 g2) = compare (g1, p1, d1) (g2, p2, d2)

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

neighbors :: Set Vec2 -> Node -> [Node]
neighbors spaces (Node pos' dir g) =
  filter
    ((`Set.member` spaces) . pos)
    [ Node (pos' + dir) dir (g + 1),
      Node pos' (rotateCW90 dir) (g + 1000),
      Node pos' (rotateCCW90 dir) (g + 1000)
    ]

dijkstra :: Set Vec2 -> Vec2 -> Map (Vec2, Vec2) Int
dijkstra spaces start =
  go
    (Map.singleton (state startNode) 0)
    (Set.singleton startNode)
  where
    state (Node p d _) = (p, d)
    startNode = Node start (Vec2 (1, 0)) 0
    go gs open =
      let current = fromMaybe (error "No path") (Set.lookupMin open)
          ns =
            filter
              ( \n -> case Map.lookup (state n) gs of
                  Nothing -> True
                  Just g' -> g n < g'
              )
              $ neighbors spaces current
          ns' = Map.fromList $ map (\n -> (state n, g n)) ns
          gs' = ns' `Map.union` gs
          open' = Set.fromList ns `Set.union` Set.delete current open
       in if Set.null open then gs else go gs' open'

tileCost :: Map (Vec2, Vec2) Int -> Vec2 -> Int
tileCost gs tile = minimum $ map snd $ Map.toList $ Map.filterWithKey (\(pos, _) _ -> pos == tile) gs

part1 :: String -> Int
part1 input =
  let (spaces, start, end) = parseInput input
      gs = dijkstra spaces start
   in tileCost gs end

part2 :: String -> Int
part2 input =
  let (spaces, start, end) = parseInput input
      gs = dijkstra spaces start
      gs' = dijkstra spaces end
      cost = 1000 + tileCost gs end
      dirs = take 4 $ iterate rotateCW90 (Vec2 (1, 0))
   in length
        $ filter
          (\pos -> or [fromJust (Map.lookup (pos, d) gs') + fromJust (Map.lookup (pos, negate d) gs) == cost | d <- dirs])
        $ Set.toList spaces
