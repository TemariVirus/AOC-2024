module Vec2 where

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

apply :: (Int -> Int) -> Vec2 -> Vec2
apply f (Vec2 (x, y)) = Vec2 (f x, f y)

apply2 :: (Int -> Int -> Int) -> Vec2 -> Vec2 -> Vec2
apply2 f (Vec2 (x1, y1)) (Vec2 (x2, y2)) = Vec2 (f x1 x2, f y1 y2)

orthogonal :: Vec2 -> Bool
orthogonal (Vec2 (x, y)) = x == 0 || y == 0
