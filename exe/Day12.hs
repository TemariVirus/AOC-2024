module Day12 where

import Data.List (group)
import Data.Maybe (fromJust, isNothing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..))

type Farm = Seq (Seq Char)

parseInput :: String -> Farm
parseInput input = Seq.fromList $ map Seq.fromList $ lines input

neighbors :: Vec2 -> [Vec2]
neighbors (Vec2 (x, y)) = [Vec2 (x - 1, y), Vec2 (x + 1, y), Vec2 (x, y - 1), Vec2 (x, y + 1)]

(!?) :: Seq (Seq a) -> Vec2 -> Maybe a
s !? Vec2 (x, y) = Seq.lookup x =<< Seq.lookup y s

floodFill :: Farm -> Set Vec2 -> Vec2 -> (Set Vec2, Int, Int)
floodFill farm visited pos =
  foldr go (Set.insert pos visited, 1, 0) (neighbors pos)
  where
    c = fromJust (farm !? pos)
    go pos' (visited', area, perim)
      | isNothing c' = (visited', area, perim + 1)
      | c /= fromJust c' = (visited', area, perim + 1)
      | pos' `Set.member` visited' = (visited', area, perim)
      | otherwise =
          let (visited'', area', perim') = floodFill farm (Set.insert pos' visited') pos'
           in (visited'', area + area', perim + perim')
      where
        c' = farm !? pos'

prices :: Farm -> [Int]
prices farm = snd $ foldr go (Set.empty, []) [Vec2 (x, y) | y <- [0 .. Seq.length farm - 1], x <- [0 .. Seq.length (Seq.index farm 0) - 1]]
  where
    go pos (visited, ps) =
      if pos `Set.member` visited
        then (visited, ps)
        else
          let (visited', area, perim) = floodFill farm visited pos
           in (visited', (area * perim) : ps)

part1 :: String -> Int
part1 = sum . prices . parseInput

bounds :: Set Vec2 -> (Int, Int, Int, Int)
bounds pots = (minimum xs, maximum xs, minimum ys, maximum ys)
  where
    xs = map (\(Vec2 (x, _)) -> x) $ Set.toList pots
    ys = map (\(Vec2 (_, y)) -> y) $ Set.toList pots

countEdges :: Set Vec2 -> [[Vec2]] -> Int
countEdges pots points =
  let groups = group $ map (map (`Set.member` pots)) points
      edges = filter (((== 1) . length . filter id) . head) groups
   in length edges

pricesBulk :: Farm -> [Int]
pricesBulk farm = snd $ foldr go (Set.empty, []) [Vec2 (x, y) | y <- [0 .. Seq.length farm - 1], x <- [0 .. Seq.length (Seq.index farm 0) - 1]]
  where
    go pos (visited, ps) =
      if pos `Set.member` visited
        then (visited, ps)
        else
          let (visited', area, _) = floodFill farm visited pos
              pots = visited' `Set.difference` visited
              (minX, maxX, minY, maxY) = bounds pots
              rowEdges =
                sum
                  [ let points = map (\x -> [Vec2 (x, y), Vec2 (x, y + 1)]) [minX - 1 .. maxX + 1]
                     in countEdges pots points
                    | y <- [minY - 1 .. maxY + 1]
                  ]
              colEdges =
                sum
                  [ let points = map (\y -> [Vec2 (x, y), Vec2 (x + 1, y)]) [minY - 1 .. maxY + 1]
                     in countEdges pots points
                    | x <- [minX - 1 .. maxX + 1]
                  ]
           in (visited', area * (rowEdges + colEdges) : ps)

part2 :: String -> Int
part2 = sum . pricesBulk . parseInput
