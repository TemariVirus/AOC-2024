module Day14 where

import Common (split)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Vec2 (Vec2 (..))
import Vec2 qualified

type Robot = (Vec2, Vec2)

parseInput :: String -> [Robot]
parseInput input = map parseLine $ lines input
  where
    parseLine s =
      case map parseVec2 $ split " " s of
        [pos, vel] -> (pos, vel)
        _ -> error "Invalid input"
    parseVec2 s =
      case map read $ split "," $ drop 2 s of
        [x, y] -> Vec2 (x, y)
        _ -> error "Invalid input"

simulate :: Vec2 -> Int -> Robot -> Vec2
simulate bounds t (pos, vel) =
  let pos' = Vec2.apply (* t) vel + pos
   in Vec2.apply2 mod pos' bounds

part1 :: String -> Int
part1 input =
  let w = 101
      h = 103
      wMid = w `div` 2
      hMid = h `div` 2
      pos' = map (simulate (Vec2 (w, h)) 100) $ parseInput input
      tl = filter (\(Vec2 (x, y)) -> x < wMid && y < hMid) pos'
      tr = filter (\(Vec2 (x, y)) -> x > wMid && y < hMid) pos'
      bl = filter (\(Vec2 (x, y)) -> x < wMid && y > hMid) pos'
      br = filter (\(Vec2 (x, y)) -> x > wMid && y > hMid) pos'
   in product $ map length [tl, tr, bl, br]

hasTreeBox :: Set Vec2 -> Bool
hasTreeBox s =
  let xs = Set.map (\(Vec2 (x, _)) -> x) s
      ys = Set.map (\(Vec2 (_, y)) -> y) s
      hasLineX x' = Set.size (Set.filter (\(Vec2 (x, _)) -> x == x') s) >= 31
      hasLineY y' = Set.size (Set.filter (\(Vec2 (_, y)) -> y == y') s) >= 31
      linesX = filter hasLineX [minimum xs .. maximum xs]
      linesY = filter hasLineY [minimum ys .. maximum ys]
   in length linesX >= 2 && length linesY >= 2

part2 :: String -> Int
part2 input =
  let w = 101
      h = 103
      input' = parseInput input
      simulate' t' = Set.fromList $ map (simulate (Vec2 (w, h)) t') input'
      trees = map simulate' [0 ..]
   in fromJust $ findIndex hasTreeBox trees
