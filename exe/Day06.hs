{-# LANGUAGE LambdaCase #-}

module Day06 where

import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set

newtype Vec2 = Vec2 (Int, Int) deriving (Show, Eq)

instance Ord Vec2 where
  -- compare doesn't really make sense for Vec2, but it's needed for Set
  compare (Vec2 (x1, y1)) (Vec2 (x2, y2)) = compare (x1, y1) (x2, y2)
  min = apply2 min
  max = apply2 max

instance Num Vec2 where
  (+) = apply2 (+)
  (*) = apply2 (*)
  abs = apply abs
  signum = apply signum
  fromInteger _ = error "Not implemented"
  negate = apply negate

up :: Vec2
up = Vec2 (0, -1)

turnRight90 :: Vec2 -> Vec2
turnRight90 (Vec2 (x, y)) = Vec2 (-y, x)

apply :: (Int -> Int) -> Vec2 -> Vec2
apply f (Vec2 (x, y)) = Vec2 (f x, f y)

apply2 :: (Int -> Int -> Int) -> Vec2 -> Vec2 -> Vec2
apply2 f (Vec2 (x1, y1)) (Vec2 (x2, y2)) = Vec2 (f x1 x2, f y1 y2)

data Guard = Guard
  { pos :: Vec2,
    dir :: Vec2
  }
  -- Ord doesn't really make sense for Guard, but it's needed for Set
  deriving (Show, Eq, Ord)

data Step = Walk Vec2 | Turn Guard

parseInput :: String -> (Int, Int, Set Vec2, Guard)
parseInput input =
  let width = length $ head $ lines input
      height = length $ lines input
      chars =
        [ (Vec2 (x, y), c)
          | (y, line) <- zip [0 ..] (lines input),
            (x, c) <- zip [0 ..] line
        ]
      obstructions = map fst $ filter ((== '#') . snd) chars
      guardPos = fst . head $ filter ((== '^') . snd) chars
   in -- Assume the guard always starts facing up
      (width, height, Set.fromList obstructions, Guard guardPos up)

getSteps :: Int -> Int -> Guard -> Set Vec2 -> [Step]
getSteps w h (Guard {pos = pos@(Vec2 (x, y)), dir = d}) obs
  | x < 0 || y < 0 || x >= w || y >= h = []
  | otherwise = step : getSteps w h (Guard {pos = pos', dir = d'}) obs
  where
    canMove = not $ (pos + d) `Set.member` obs
    d' = if canMove then d else turnRight90 d
    pos' = if canMove then pos + d else pos
    step = if canMove then Walk pos else Turn (Guard pos d')

part1 :: String -> Int
part1 input =
  let (w, h, obs, guard) = parseInput input
   in (Set.size . Set.fromList) [s | Walk s <- getSteps w h guard obs]

hasLoop :: Int -> Int -> Guard -> Set Vec2 -> Bool
hasLoop w h guard obs =
  ( \case
      (_, Just True) : _ -> True
      _ -> False
  )
    . dropWhile (isNothing . snd)
    . scanl
      ( \(v, _) s -> case s of
          Walk _ -> (v, Nothing)
          Turn g -> if g `Set.member` v then (v, Just True) else (Set.insert g v, Nothing)
      )
      (Set.empty, Nothing)
    $ getSteps w h guard obs

part2 :: String -> Int
part2 input =
  let (w, h, obs, guard) = parseInput input
      hasLoop' = hasLoop w h guard
      path = Set.toList $ Set.fromList [s | Walk s <- getSteps w h guard obs]
   in length
        [ p
          | p <- path,
            not $ p `Set.member` obs,
            let obs' = Set.insert p obs
             in hasLoop' obs'
        ]
