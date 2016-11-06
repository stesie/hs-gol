module Rules (isAlive) where

isAlive :: Bool -> Int -> Bool
isAlive alive neighbours
  | neighbours == 2 = alive
  | neighbours == 3 = True
  | otherwise       = False
