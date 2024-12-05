module Day05 where

import Common (splitOn)
import Data.List (sort, sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

type Graph = [(Int, Int)]

parseOrdering :: String -> (Int, Int)
parseOrdering line =
  case (map read . splitOn "|") line of
    [a, b] -> (a, b)
    _ -> error "Invalid input"

parseUpdate :: String -> [Int]
parseUpdate = map read . splitOn ","

parseInput :: String -> (Graph, [[Int]])
parseInput input = case (map lines . splitOn "\n\n") input of
  [lines1, lines2] ->
    let orderings = map parseOrdering lines1
        updates = map parseUpdate lines2
     in (orderings, updates)
  _ -> error "Invalid input"

filterEdges :: Graph -> [Int] -> Graph
filterEdges graph nodes = filter (\(x, y) -> x `elem` nodes && y `elem` nodes) graph

totalOrdering :: Graph -> [Int]
totalOrdering orderings =
  -- Each value of the map is the set of values that come after the corresponding key
  let orderingMap =
        foldr
          ( \(before, after) acc ->
              let -- Update all sets that contain before with after
                  updateBeforeDeps =
                    foldr
                      ( \(key, value) acc2 ->
                          let updated = Set.union value (Set.singleton after)
                           in ( if before `Set.member` value
                                  then Map.insert key updated acc2
                                  else acc2
                              )
                      )
                      acc
                      (Map.toList acc)
                  -- Update before's set with after
                  updateBefore = Map.insertWith Set.union before (Set.singleton after) updateBeforeDeps
               in -- Create empty set set for after if it doesn't exist yet
                  Map.insertWith Set.union after Set.empty updateBefore
          )
          Map.empty
          orderings
      -- Sort by the sizes of the sets
      sorted = sortOn (negate . Set.size . snd) $ Map.toList orderingMap
   in -- Only keep the keys
      map fst sorted

nodeToIndex :: Graph -> [Int] -> [Int]
nodeToIndex graph update =
  let graph' = filterEdges graph update
      indexMap = Map.fromList $ zip (totalOrdering graph') [0 ..]
      lookupAssume = fromMaybe (error "Unknown page") . (`Map.lookup` indexMap)
   in map lookupAssume update

isSorted :: (Ord a) => [a] -> Bool
isSorted arr = case arr of
  x : y : _ -> x <= y && isSorted (tail arr)
  _ -> True

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: String -> Int
part1 input =
  let (graph, updates) = parseInput input
      sorted = filter (isSorted . nodeToIndex graph) updates
   in sum $ map middle sorted

filterBy :: [Bool] -> [a] -> [a]
filterBy bools arr = map snd $ filter fst $ zip bools arr

indexToNode :: Graph -> [Int] -> [Int] -> [Int]
indexToNode graph update indexed =
  let graph' = filterEdges graph update
      indexUnmap = Map.fromList $ zip [0 ..] (totalOrdering graph')
      lookupAssume = fromMaybe (error "Unknown index") . (`Map.lookup` indexUnmap)
   in map lookupAssume indexed

part2 :: String -> Int
part2 input =
  let (graph, updates) = parseInput input
      indexed = map (nodeToIndex graph) updates
      unsorteds = map (not . isSorted) indexed
      sorted = map sort $ filterBy unsorteds indexed
      updates' = filterBy unsorteds updates
      unindexed = zipWith (indexToNode graph) updates' sorted
   in sum $ map middle unindexed
