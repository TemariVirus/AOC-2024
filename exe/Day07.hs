module Day07 where

import Common (splitOn)
import Data.Int (Int64)

type Op = Int64 -> Int64 -> Int64

parseInput :: String -> [(Int64, [Int64])]
parseInput input =
  [ (read v, map read $ words nums)
    | line <- lines input,
      [v, nums] <- [splitOn ": " line]
  ]

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations digits len =
  [d : comb | comb <- combinations digits (len - 1), d <- digits]

satisfiable :: [Op] -> Int64 -> [Int64] -> Bool
satisfiable _ _ [] = False
satisfiable ops total (x : xs) =
  elem total
    $ map
      (foldl (\acc (n, op) -> acc `op` n) x . zip xs)
    $ combinations ops (length xs)

part1 :: String -> Int64
part1 input =
  let sat = satisfiable [(+), (*)]
   in sum [total | (total, nums) <- parseInput input, sat total nums]

-- Meow
cat :: Int64 -> Int64 -> Int64
cat a b = b + a * 10 ^ length (show b)

part2 :: String -> Int64
part2 input =
  let sat = satisfiable [(+), (*), cat]
   in sum [total | (total, nums) <- parseInput input, sat total nums]
