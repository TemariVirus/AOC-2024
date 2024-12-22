module Day22 where

import Common (toPair, windows)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)

parseInput :: String -> [Int64]
parseInput = map read . lines

next :: Int64 -> Int64
next x =
  let mixPrune a b = (a `xor` b) .&. 0xFFFFFF
      x' = x `mixPrune` (x `shiftL` 6)
      x'' = x' `mixPrune` (x' `shiftR` 5)
   in x'' `mixPrune` (x'' `shiftL` 11)

part1 :: String -> Int64
part1 = sum . map ((!! 2000) . iterate next) . parseInput

changesToPrice :: Int64 -> Map [Int64] Int64
changesToPrice x =
  let prices = take 2001 $ map (`mod` 10) $ iterate next x
      diffs = map (uncurry (-) . swap . toPair) $ windows 2 prices
   in -- Reverse list to keep the first price for duplicate keys
      Map.fromList $ reverse $ zip (windows 4 diffs) (drop 4 prices)

part2 :: String -> Int64
part2 =
  maximum
    . map snd
    . Map.toList
    . Map.unionsWith (+)
    . map changesToPrice
    . parseInput
