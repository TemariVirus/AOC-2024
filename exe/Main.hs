module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (DiffTime, picosecondsToDiffTime)
import Day21 (part1, part2)
import System.CPUTime (getCPUTime)

timeIt :: (MonadIO m) => m a -> m (DiffTime, a)
timeIt ioa = do
  t1 <- liftIO getCPUTime
  a <- ioa
  t2 <- liftIO getCPUTime
  return (picosecondsToDiffTime (t2 - t1), a)

timeItPure :: (Show b) => (a -> b) -> a -> IO ()
timeItPure f x = do
  (t, ans) <- timeIt $ let y = f x in y `seq` return y
  putStrLn $ "Answer: " ++ show ans
  putStrLn $ "Time taken: " ++ show t

main :: IO ()
main = do
  input <- readFile "inputs/21.txt"
  timeItPure part1 input
  timeItPure part2 input
