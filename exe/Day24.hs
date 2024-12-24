module Day24 where

import Common (split)
import Data.Bits (shiftL, xor, (.&.), (.|.))
import Data.Map (Map, (!?))
import Data.Map qualified as Map

type Gate = (String, String, String, String)

parseInput :: String -> (Map String Int, [Gate])
parseInput input =
  let [wires, gates] = split "\n\n" input
   in ( Map.fromList $ map ((\[k, v] -> (k, read v)) . split ": ") $ lines wires,
        map ((\[in1, op, in2, _, out] -> (op, in1, in2, out)) . split " ") $ lines gates
      )

process :: Map String Int -> [Gate] -> Map String Int
process wires [] = wires
process wires gates =
  uncurry process $
    foldr
      ( \gate@(op, in1, in2, out) (wires', gates') ->
          case (wires' !? in1, wires' !? in2) of
            (Just v1, Just v2) ->
              ( Map.insert
                  out
                  ( case op of
                      "AND" -> v1 .&. v2
                      "OR" -> v1 .|. v2
                      "XOR" -> v1 `xor` v2
                      _ -> error "Invalid operation"
                  )
                  wires',
                gates'
              )
            _ -> (wires', gate : gates')
      )
      (wires, [])
      gates

part1 :: String -> Int
part1 =
  foldl (\acc (_, b) -> acc `shiftL` 1 .|. b) 0
    . Map.toDescList
    . Map.filterWithKey (\(k : _) _ -> k == 'z')
    . uncurry process
    . parseInput

-- part2 :: String -> Int
part2 input = 0
