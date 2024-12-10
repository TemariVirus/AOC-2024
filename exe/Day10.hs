module Day10 where

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

parseInput :: String -> Seq (Seq Int)
parseInput input = Seq.fromList $ map (Seq.fromList . map digitToInt) $ lines input

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

(!?) :: Seq (Seq a) -> (Int, Int) -> Maybe a
s !? (x, y) = Seq.lookup x =<< Seq.lookup y s

hike :: Seq (Seq Int) -> (Int, Int) -> Set.Set (Int, Int)
hike rows pos =
  case rows !? pos of
    Nothing -> Set.empty
    Just 9 -> Set.singleton pos
    Just h -> foldl1 Set.union . map (go h) $ neighbors pos
  where
    go h pos' =
      case rows !? pos' of
        Nothing -> Set.empty
        Just h' -> if h + 1 == h' then hike rows pos' else Set.empty

part1 :: String -> Int
part1 input =
  let rows = parseInput input
   in sum
        [ Set.size (hike rows (x, y))
          | (y, row) <- zip [0 ..] $ toList rows,
            (x, h) <- zip [0 ..] $ toList row,
            h == 0
        ]

hike2 :: Seq (Seq Int) -> (Int, Int) -> Set.Set [(Int, Int)]
hike2 = hike2' []

hike2' :: [(Int, Int)] -> Seq (Seq Int) -> (Int, Int) -> Set.Set [(Int, Int)]
hike2' acc rows pos =
  case rows !? pos of
    Nothing -> Set.empty
    Just 9 -> Set.singleton acc'
    Just h -> foldl1 Set.union . map (go h) $ neighbors pos
  where
    acc' = pos : acc
    go h pos' =
      case rows !? pos' of
        Nothing -> Set.empty
        Just h' -> if h + 1 == h' then hike2' acc' rows pos' else Set.empty

part2 :: String -> Int
part2 input =
  let rows = parseInput input
   in Set.size $
        foldl
          Set.union
          Set.empty
          [ hike2 rows (x, y)
            | (y, row) <- zip [0 ..] $ toList rows,
              (x, h) <- zip [0 ..] $ toList row,
              h == 0
          ]
