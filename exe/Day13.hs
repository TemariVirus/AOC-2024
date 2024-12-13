module Day13 where

import Common (split)

data ClawMachine = ClawMachine
  { a :: (Int, Int),
    b :: (Int, Int),
    prize :: (Int, Int)
  }
  deriving (Show)

parseClawMachine :: String -> ClawMachine
parseClawMachine input =
  let parsed =
        map
          (map (read . drop 2) . split ", " . (!! 1) . split ": ")
          $ lines input
   in case parsed of
        [[ax, ay], [bx, by], [x, y]] -> ClawMachine {a = (ax, ay), b = (bx, by), prize = (x, y)}
        _ -> error "Invalid input"

parseInput :: String -> [ClawMachine]
parseInput = map parseClawMachine . split "\n\n"

-- Solves for A and B in this system of equations:
-- A * ax + B * xb = x
-- A * ay + B * by = y
solvePresses :: ClawMachine -> Maybe (Int, Int)
solvePresses ClawMachine {a = (ax, ay), b = (bx, by), prize = (x, y)}
  -- AoC was nice enough to make this line unreachable 🥳
  | det == 0 = error "Multiple solutions?"
  | solvable = Just (a, b)
  | otherwise = Nothing
  where
    det = ax * by - ay * bx
    aNum = x * by - y * bx
    bNum = ax * y - ay * x
    a = aNum `div` det
    b = bNum `div` det
    -- Make sure A and B are integers
    solvable = a * det == aNum && b * det == bNum

part1 :: String -> Int
part1 input =
  sum
    [ 3 * a + 1 * b
      | Just (a, b) <- map solvePresses $ parseInput input
    ]

parseInput2 :: String -> [ClawMachine]
parseInput2 input =
  map
    ( \ClawMachine {a = a, b = b, prize = (x, y)} ->
        ClawMachine {a = a, b = b, prize = (x + 10000000000000, y + 10000000000000)}
    )
    $ parseInput input

part2 :: String -> Int
part2 input =
  sum
    [ 3 * a + 1 * b
      | Just (a, b) <- map solvePresses $ parseInput2 input
    ]
