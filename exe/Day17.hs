module Day17 where

import Common (split)
import Data.Bits (Bits (shiftR), xor)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

type Regs = (Int, Int, Int)

parseInput :: String -> (Regs, [Int])
parseInput input = case lines input of
  lineA : lineB : lineC : _ : lineProgram : _ ->
    ( (parseReg lineA, parseReg lineB, parseReg lineC),
      map read $ split "," $ split ": " lineProgram !! 1
    )
  _ -> error "Invalid input"
  where
    parseReg = read . (!! 1) . split ": "

run :: Seq Int -> Regs -> Int -> [Int]
run program regs@(a, b, c) ip
  | ip >= Seq.length program = []
  | opcode == 0 = run program (adv, b, c) ip'
  | opcode == 1 = run program (a, b `xor` operandL, c) ip'
  | opcode == 2 = run program (a, operandC `mod` 8, c) ip'
  | opcode == 3 = if a == 0 then run program regs ip' else run program regs operandL
  | opcode == 4 = run program (a, b `xor` c, c) ip'
  | opcode == 5 = operandC `mod` 8 : run program regs ip'
  | opcode == 6 = run program (a, adv, c) ip'
  | opcode == 7 = run program (a, b, adv) ip'
  | otherwise = error "Invalid opcode"
  where
    opcode = program `Seq.index` ip
    operandL = program `Seq.index` (ip + 1)
    operandC = case operandL of
      4 -> a
      5 -> b
      6 -> c
      7 -> error "Invalid operand"
      x -> x
    ip' = ip + 2
    adv = a `shiftR` operandC

part1 :: String -> String
part1 input =
  let (regs, program) = parseInput input
      outputs = run (Seq.fromList program) regs 0
   in init $ concatMap ((++ ",") . show) outputs

unrun :: [Int] -> Int
unrun program = case program !! (length program - 2) of
  3 -> fromJust $ go 0 (reverse program)
  _ -> error "Unable to haldle this case"
  where
    -- Discard jnz instruction at the end
    program' = Seq.fromList $ take (length program - 2) program
    run' a = head $ run program' (a, 0, 0) 0
    go :: Int -> [Int] -> Maybe Int
    go a [] = Just a
    go a (x : xs) =
      let valids =
            [ a''
              | b <- [0 .. 7],
                a' <- [a * 8 + b],
                run' a' == x,
                Just a'' <- [go a' xs]
            ]
       in if null valids then Nothing else Just (head valids)

part2 :: String -> Int
part2 input =
  let (_, program) = parseInput input
   in unrun program
