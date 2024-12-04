module Day01 where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Map qualified as Map

parseLinePair :: String -> (Int, Int)
parseLinePair line =
  let w = words line
   in (read $ w !! 0, read $ w !! 1)

parsePairs :: String -> ([Int], [Int])
parsePairs input =
  let pairs = map parseLinePair $ lines input
   in (map fst pairs, map snd pairs)

pairDiffSum :: ([Int], [Int]) -> Int -> Int
pairDiffSum ([], []) acc = acc
pairDiffSum (x : xs, y : ys) acc = pairDiffSum (xs, ys) (acc + abs (x - y))
pairDiffSum _ _ = error "pairDiffSum: lists are not the same length"

part1 :: String -> Int
part1 input =
  let pairs = parsePairs input
      sorted = Data.Bifunctor.bimap sort sort pairs
   in pairDiffSum sorted 0

getCounts :: [Int] -> Map.Map Int Int
getCounts = foldr (\x -> Map.insertWith (+) x 1) Map.empty

part2 :: String -> Int
part2 input =
  let pairs = parsePairs input
      counts = getCounts $ snd pairs
   in foldr
        ( \x acc -> case Map.lookup x counts of
            Just y -> acc + (x * y)
            Nothing -> acc
        )
        0
        $ fst pairs
