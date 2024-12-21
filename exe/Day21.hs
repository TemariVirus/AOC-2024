module Day21 where

import Common (windows)
import Data.List (elemIndex, sortOn)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Vec2 (Vec2 (..), collapse)

type Pad = Map Char Vec2

-- (0, 0) is the gap
numPad :: Pad
numPad =
  Map.fromList
    [ ('7', Vec2 (0, 3)),
      ('8', Vec2 (1, 3)),
      ('9', Vec2 (2, 3)),
      --
      ('4', Vec2 (0, 2)),
      ('5', Vec2 (1, 2)),
      ('6', Vec2 (2, 2)),
      --
      ('1', Vec2 (0, 1)),
      ('2', Vec2 (1, 1)),
      ('3', Vec2 (2, 1)),
      --
      ('0', Vec2 (1, 0)),
      ('A', Vec2 (2, 0))
    ]

-- (0, 0) is the gap
dPad :: Pad
dPad =
  Map.fromList
    [ ('^', Vec2 (1, 0)),
      ('A', Vec2 (2, 0)),
      --
      ('<', Vec2 (0, -1)),
      ('v', Vec2 (1, -1)),
      ('>', Vec2 (2, -1))
    ]

moveToChar :: Int -> Int -> Char
moveToChar (-1) 0 = '<'
moveToChar 1 0 = '>'
moveToChar 0 (-1) = 'v'
moveToChar 0 1 = '^'
moveToChar _ _ = error "Invalid move"

shortestPath :: Vec2 -> Vec2 -> String
shortestPath from to =
  -- The bounding box of from and to contains the gap if they have 0s in the x any y components
  let avoidGap = collapse (+) (from * to) == 0
      Vec2 (x, y) = to - from
      xs = replicate (abs x) $ moveToChar (signum x) 0
      ys = replicate (abs y) $ moveToChar 0 (signum y)
   in -- If one of the shortest paths goes over the gap, then to avoid the gap, we need to go:
      --   on the numpad -> ^ before <, > before v
      --   on the d-pad  -> > before ^, v before <
      -- Combining those constraints gives us ">^v<"
      --
      -- If no shortest paths go over the gap, then... IDK
      -- Some trial and error shows that "<v^>" works.
      -- It can be interpreted as prioritizing d-pad keys further away from 'A', with a bias for vertical moves.
      sortOn (`elemIndex` if avoidGap then ">^v<" else "<v^>") $ xs ++ ys

shortestSeq :: Pad -> String -> [String]
shortestSeq pad =
  map (\[p', p] -> shortestPath p' p ++ "A")
    . windows 2
    . map (pad !)
    . ('A' :)

shortestSeqCounts :: Pad -> Map String Int -> Map String Int
shortestSeqCounts pad =
  Map.fromListWith (+)
    . concatMap (\(k, v) -> map (,v) $ shortestSeq pad k)
    . Map.toList

part :: Int -> String -> Int
part robots input =
  let codes = lines input
      numSeq = shortestSeqCounts numPad
      dSeq = shortestSeqCounts dPad
      codeLen = sum . map (\(k, v) -> length k * v) . Map.toList . (!! robots) . iterate dSeq . numSeq . (`Map.singleton` 1)
   in sum $ map (\code -> codeLen code * read (init code)) codes

part1 :: String -> Int
part1 = part 2

part2 :: String -> Int
part2 = part 25
