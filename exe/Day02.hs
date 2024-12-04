module Day02 where

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

isReportGradual :: [Int] -> Bool
isReportGradual xs = case xs of
  (a : b : _) ->
    let diff = abs (a - b)
     in diff >= 1 && diff <= 3 && isReportGradual (tail xs)
  _ -> True

isReportMonotonous :: [Int] -> Bool
isReportMonotonous xs = case xs of
  (a : b : c : _) -> signum (a - b) == signum (b - c) && isReportMonotonous (tail xs)
  _ -> True

isReportSafe :: [Int] -> Bool
isReportSafe report = isReportGradual report && isReportMonotonous report

part1 :: String -> Int
part1 input = length . filter id $ map isReportSafe $ parseInput input

removeOneOrNone :: [a] -> [[a]]
removeOneOrNone [] = [[]]
removeOneOrNone (x : xs) = xs : map (x :) (removeOneOrNone xs)

part2 :: String -> Int
part2 input = length . filter id $ map (any isReportSafe . removeOneOrNone) $ parseInput input
