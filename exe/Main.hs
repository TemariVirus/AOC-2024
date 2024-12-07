module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (DiffTime, picosecondsToDiffTime)
import Day04 (part1, part2)
import System.CPUTime

timeIt :: (MonadIO m) => m a -> m (DiffTime, a)
timeIt ioa = do
  t1 <- liftIO getCPUTime
  a <- ioa
  t2 <- liftIO getCPUTime
  return (picosecondsToDiffTime (t2 - t1), a)

timeItPure :: (a -> b) -> a -> IO (DiffTime, b)
timeItPure f x = timeIt $ let y = f x in y `seq` return y

main :: IO ()
main = do
  input <- readFile "inputs/04.txt"

  part1Ans <- timeItPure part1 input
  putStrLn $ "Answer: " ++ show (snd part1Ans)
  putStrLn $ "Time taken: " ++ show (fst part1Ans)

  part2Ans <- timeItPure part2 input
  putStrLn $ "Answer: " ++ show (snd part2Ans)
  putStrLn $ "Time taken: " ++ show (fst part2Ans)
