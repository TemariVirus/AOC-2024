module Day15 where

import Common (split)
import Data.Map (Map)
import Data.Map qualified as Map
import Vec2 (Vec2 (..))
import Vec2 qualified

type Maze = Map Vec2 Char

parseInput :: String -> (Vec2, Maze, [Vec2])
parseInput input =
  case split "\n\n" input of
    [mazeStr, movesStr] ->
      let maze =
            [ (Vec2 (x, y), c)
              | (y, line) <- zip [0 ..] $ lines mazeStr,
                (x, c) <- zip [0 ..] line
            ]
          robot = fst $ head $ filter ((== '@') . snd) maze
          tiles =
            Map.fromList
              [ (pos, c)
                | (pos, c) <- maze,
                  c `elem` "#O"
              ]
          moves =
            [ case c of
                '<' -> Vec2 (-1, 0)
                '>' -> Vec2 (1, 0)
                '^' -> Vec2 (0, -1)
                'v' -> Vec2 (0, 1)
                _ -> error "Unreachable"
              | c <- movesStr,
                c `elem` "<>^v"
            ]
       in (robot, tiles, moves)
    _ -> error "Invalid input"

tryMoveBox :: Vec2 -> Maze -> Vec2 -> Maybe Maze
tryMoveBox box maze move =
  case Map.lookup box' maze of
    Nothing -> Just (Map.insert box' 'O' maze)
    Just 'O' -> tryMoveBox box' maze move
    _ -> Nothing
  where
    box' = box + move

moveRobot :: Vec2 -> Maze -> Vec2 -> (Vec2, Maze)
moveRobot robot maze move =
  case Map.lookup robot' maze of
    Nothing -> (robot', maze)
    Just 'O' -> case tryMoveBox robot' maze move of
      Nothing -> (robot, maze)
      Just maze' -> (robot', Map.delete robot' maze')
    _ -> (robot, maze)
  where
    robot' = robot + move

part :: (String -> (Vec2, Maze, [Vec2])) -> (Vec2 -> Maze -> Vec2 -> (Vec2, Maze)) -> String -> Int
part parse moveFn input =
  let (robot, tiles, moves) = parse input
      (_, tiles') = foldl (\(robot', maze) move -> moveFn robot' maze move) (robot, tiles) moves
      boxes = Map.toList $ Map.filter (== 'O') tiles'
   in sum $ map ((\(Vec2 (x, y)) -> 100 * y + x) . fst) boxes

part1 :: String -> Int
part1 = part parseInput moveRobot

parseInput2 :: String -> (Vec2, Maze, [Vec2])
parseInput2 input =
  case split "\n\n" input of
    [mazeStr, movesStr] ->
      let maze =
            [ (Vec2 (2 * x, y), c)
              | (y, line) <- zip [0 ..] $ lines mazeStr,
                (x, c) <- zip [0 ..] line
            ]
          robot = fst $ head $ filter ((== '@') . snd) maze
          tiles =
            Map.fromList
              [ (pos, c)
                | (pos, c) <- maze,
                  c `elem` "#O"
              ]
          moves =
            [ case c of
                '<' -> Vec2 (-1, 0)
                '>' -> Vec2 (1, 0)
                '^' -> Vec2 (0, -1)
                'v' -> Vec2 (0, 1)
                _ -> error "Unreachable"
              | c <- movesStr,
                c `elem` "<>^v"
            ]
       in (robot, tiles, moves)
    _ -> error "Invalid input"

tryMoveBox2H :: Vec2 -> Maze -> Vec2 -> Maybe Maze
tryMoveBox2H box maze move =
  case Map.lookup check maze of
    Nothing -> Just maze'
    Just 'O' -> tryMoveBox2H check maze' move
    _ -> Nothing
  where
    box' = box + move
    check = box + Vec2.apply (* 2) move
    maze' = Map.insert box' 'O' $ Map.delete box maze

tryMoveBox2V :: Vec2 -> Maze -> Vec2 -> Maybe Maze
tryMoveBox2V box maze move
  | Just '#' `elem` checks = Nothing
  | otherwise =
      moveBox
        <$> foldr
          ( \(pos, check) acc ->
              case (check, acc) of
                (Nothing, _) -> acc
                (_, Nothing) -> Nothing
                (_, Just maze') -> tryMoveBox2V pos maze' move
          )
          (Just maze)
          (zip moves checks)
  where
    box'l = box' + Vec2 (-1, 0)
    box' = box + move
    box'r = box' + Vec2 (1, 0)
    moves = [box'l, box', box'r]
    checks = map (`Map.lookup` maze) moves
    moveBox = Map.insert box' 'O' . Map.delete box

moveRobot2 :: Vec2 -> Maze -> Vec2 -> (Vec2, Maze)
moveRobot2 robot maze move =
  let maze' = case move of
        Vec2 (-1, 0) -> goH (robot' + Vec2 (-1, 0))
        Vec2 (1, 0) -> goH robot'
        _ ->
          let moves = [robot' + Vec2 (-1, 0), robot']
              checks = map (`Map.lookup` maze) moves
           in if Just '#' `elem` checks
                then Nothing
                else
                  foldr
                    ( \(pos, check) acc ->
                        case (check, acc) of
                          (Nothing, _) -> acc
                          (_, Nothing) -> Nothing
                          (_, Just maze'') -> tryMoveBox2V pos maze'' move
                    )
                    (Just maze)
                    (zip moves checks)
   in case maze' of
        Nothing -> (robot, maze)
        Just maze'' -> (robot', maze'')
  where
    robot' = robot + move
    goH check = case Map.lookup check maze of
      Nothing -> Just maze
      Just 'O' -> tryMoveBox2H check maze move
      _ -> Nothing

part2 :: String -> Int
part2 = part parseInput2 moveRobot2
