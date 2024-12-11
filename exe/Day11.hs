module Day11 where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map

parseInput :: String -> [Int64]
parseInput = map read . words

digits :: Int64 -> Int
digits = length . show

blinks :: Map (Int, Int64) Int64 -> Int -> Int64 -> (Map (Int, Int64) Int64, Int64)
blinks m 0 _ = (m, 1)
blinks m steps stone = case Map.lookup key m of
  Just res -> (m, res)
  Nothing -> (newM, res')
  where
    key = (steps, stone)
    l = digits stone
    base = 10 ^ (l `div` 2)
    steps' = steps - 1
    (m''', res')
      | stone == 0 = blinks m steps' 1
      | even l =
          let (m', left) = blinks m steps' (stone `div` base)
              (m'', right) = blinks m' steps' (stone `mod` base)
           in (m'', left + right)
      | otherwise = blinks m steps' (stone * 2024)
    newM = Map.insert key res' m'''

part1 :: String -> Int64
part1 input = sum $ map (snd . blinks Map.empty 25) (parseInput input)

part2 :: String -> Int64
part2 input = sum $ map (snd . blinks Map.empty 75) (parseInput input)
