module Day03 where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA

mulRegex :: String
mulRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

parseMuls :: String -> [(Int, Int)]
parseMuls input =
  let matches = getAllTextMatches (input =~ mulRegex) :: [String]
      submatches = map (=~ mulRegex) matches :: [(String, String, String, [String])]
   in map
        ( \(_, _, _, xs) -> case xs of
            [a, b] -> (read a, read b)
            _ -> error "Invalid input"
        )
        submatches

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . parseMuls

positions :: String -> String -> [Int]
positions input regex = map (\(a, _ :: Int) -> a) $ getAllMatches (input =~ regex)

maximumIndex :: (Ord a) => [a] -> Int
maximumIndex xs = snd $ maximum $ zip xs [0 ..]

enabled :: [(Int, Int)] -> [Int] -> [Int] -> [Int] -> [(Int, Int)]
enabled = enabled' True

enabled' :: Bool -> [(Int, Int)] -> [Int] -> [Int] -> [Int] -> [(Int, Int)]
enabled' _ [] _ _ _ = []
enabled' _ _ [] _ _ = []
enabled' True arr mul [] (dont : _) =
  let i = fromMaybe (length mul) $ findIndex (> dont) mul
   in take i arr
enabled' False _ _ [] _ = []
enabled' True arr _ _ [] = arr
enabled' False arr mul (do' : _) [] =
  let i = fromMaybe (length mul) $ findIndex (> do') mul
   in drop i arr
enabled' e arr@(x : xs) muls dos donts =
  case current of
    0 -> [x | e] ++ enabled' e xs (tail muls) dos donts
    1 -> enabled' True arr muls (tail dos) donts
    2 -> enabled' False arr muls dos (tail donts)
    _ -> error "Unreachable"
  where
    negHeadOrOne [] = 1
    negHeadOrOne (h : _) = -h
    current = maximumIndex [negHeadOrOne muls, negHeadOrOne dos, negHeadOrOne donts]

part2 :: String -> Int
part2 input =
  let muls = parseMuls input
      pos = positions input mulRegex
      dos = positions input "do\\(\\)"
      donts = positions input "don't\\(\\)"
   in sum . map (uncurry (*)) $ enabled muls pos dos donts
