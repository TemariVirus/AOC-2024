module Day20 where

import Common (dijkstra)
import Data.Foldable (find)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..), collapse, rotateCW90)

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
      path = Set.fromList $ map fst $ filter ((/= '#') . snd) chars
   in (path, start, end)

neighbors :: Set Vec2 -> (Int, Vec2) -> [(Int, Vec2)]
neighbors path (g, pos) =
  filter (\(_, p) -> p `Set.member` path)
    . map (\dir -> (g + 1, pos + dir))
    . take 4
    $ iterate rotateCW90 (Vec2 (0, 1))

tilesInRange :: Vec2 -> Int -> [Vec2]
tilesInRange (Vec2 (x, y)) r =
  [ Vec2 (x + dx, y + dy)
    | dx <- [-r .. r],
      dy <- [-r .. r],
      abs dx + abs dy <= r
  ]

part :: Int -> String -> Int
part r input =
  let (path, start, _) = parseInput input
      costs = dijkstra (neighbors path) start
   in length
        [ Nothing
          | (p, g) <- Map.toList costs,
            p' <- filter (`Set.member` path) $ tilesInRange p r,
            let g' = costs Map.! p'
                dist = collapse (+) $ abs (p - p')
             in g - g' - dist >= 100
        ]

part1 :: String -> Int
part1 = part 2

part2 :: String -> Int
part2 = part 20
