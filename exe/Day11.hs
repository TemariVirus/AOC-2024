module Day11 where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map

parseInput :: String -> Map Int64 Int64
parseInput = Map.fromListWith (+) . map ((,1) . read) . words

blink :: Map Int64 Int64 -> Map Int64 Int64
blink counts =
  let go stone
        | stone == 0 = [1]
        | even l = [stone `div` base, stone `mod` base]
        | otherwise = [stone * 2024]
        where
          l = length $ show stone
          base = 10 ^ (l `div` 2)
   in Map.fromListWith
        (+)
        [ (stone', count)
          | (stone, count) <- Map.toList counts,
            stone' <- go stone
        ]

part :: Map Int64 Int64 -> Int -> Int64
part counts = sum . map snd . Map.toList . (iterate blink counts !!)

part1 :: String -> Int64
part1 input = part (parseInput input) 25

part2 :: String -> Int64
part2 input = part (parseInput input) 75
