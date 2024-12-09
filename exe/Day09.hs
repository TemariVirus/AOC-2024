module Day09 where

import Common (interlace)

parseInput :: String -> ([Int], [Int])
parseInput input =
  ( [read [c] | (i, c) <- zip [0 :: Int ..] input, even i],
    [read [c] | (i, c) <- zip [0 :: Int ..] input, odd i]
  )

compress :: [[Int]] -> [Int] -> [Int] -> Int -> [Int]
compress _ _ _ 0 = []
compress (f : fs) reverseFiles empties remaining =
  let blocks = take remaining f
      remaining' = remaining - length blocks
   in blocks ++ compressReverse fs reverseFiles empties remaining'
compress _ _ _ _ = error "Invalid input"

compressReverse :: [[Int]] -> [Int] -> [Int] -> Int -> [Int]
compressReverse _ _ _ 0 = []
compressReverse files reverseFiles (e : es) remaining =
  let blocks = take (min e remaining) reverseFiles
      remaining' = remaining - length blocks
      reverseFiles' = drop (length blocks) reverseFiles
   in blocks ++ compress files reverseFiles' es remaining'
compressReverse _ _ _ _ = error "Invalid input"

part1 :: String -> Int
part1 input =
  let (files, empties) = parseInput input
      flen = sum files
      fs = [replicate f i | (i, f) <- zip [0 ..] files]
      fsReverse = concat $ reverse fs
      compressed = compress fs fsReverse empties flen
   in sum [i * fid | (i, fid) <- zip [0 ..] compressed]

contiguous :: [(Int, Int)] -> [(Int, Int)]
contiguous = reverse . contiguous' []

contiguous' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
contiguous' acc [] = acc
contiguous' acc ((_, 0) : xs) = contiguous' acc xs
contiguous' acc arr@((fid, _) : _) =
  let block = takeWhile ((== fid) . fst) arr
      len' = sum $ map snd block
      arr' = drop (length block) arr
   in contiguous' ((fid, len') : acc) arr'

findSpace :: [(Int, Int)] -> Int -> Int -> Maybe ([(Int, Int)], Int, [(Int, Int)])
findSpace = findSpace' []

findSpace' :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Maybe ([(Int, Int)], Int, [(Int, Int)])
findSpace' _ [] _ _ = Nothing
findSpace' acc (d@(-1, l) : ds) len fid = if l >= len then Just (reverse acc, l, ds) else findSpace' (d : acc) ds len fid
findSpace' acc (d@(fid', _) : ds) len fid = if fid' == fid then Nothing else findSpace' (d : acc) ds len fid

compress2 :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
compress2 disk' (fid, len) = case findSpace disk' len fid of
  Just (before, l, after) ->
    let after' = map (\x@(fid', _) -> if fid == fid' then (-1, len) else x) after
     in contiguous
          ( before
              ++ [(fid, len), (-1, l - len)]
              ++ after'
          )
  Nothing -> disk'

part2 :: String -> Int
part2 input =
  let (files, empties) = parseInput input
      files' = zip [0 :: Int ..] files
      -- Empty files have id -1
      disk = interlace files' (map (-1,) empties)
      compressed = foldl compress2 disk (reverse files')
      compressed' = [x | (f, l) <- compressed, x <- replicate l f]
   in sum [i * fid | (i, fid) <- zip [0 ..] compressed', fid /= -1]
