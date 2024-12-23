module Day23 where

import Common (pairs, split)
import Data.List (intercalate, nub, sort)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set, member)
import Data.Set qualified as Set

parseInput :: String -> Map String (Set String)
parseInput input =
  Map.fromListWith
    Set.union
    [ pair
      | line <- lines input,
        [a, b] <- [split "-" line],
        pair <- [(a, Set.singleton b), (b, Set.singleton a)]
    ]

part1 :: String -> Int
part1 input =
  let connections = parseInput input
   in length . nub . concatMap snd . Map.toList
        $ Map.mapWithKey
          ( \k cs ->
              [ sort [k, a, b]
                | (a, b) <- pairs $ Set.elems cs,
                  b `member` (connections ! a)
              ]
          )
        $ Map.filterWithKey (\k _ -> head k == 't') connections

part2 :: String -> String
part2 input =
  let connections = parseInput input
      computers = Map.keys connections
      growStronglyConnected groups =
        Set.fromList
          [ sort (com : grp)
            | grp <- Set.elems groups,
              com <- computers,
              com `notElem` grp,
              all (\g -> com `member` (connections ! g)) grp
          ]
      stronglyConnected = iterate growStronglyConnected $ Set.fromList (map (: []) computers)
   in intercalate "," . head . Set.elems . last . takeWhile (not . null) $ stronglyConnected
