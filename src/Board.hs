module Board (Cell, Board (..), isEmpty, livingNeighbours) where

type Cell = (Integer, Integer)

data Board = Board Integer [Cell]
  deriving (Eq, Show)

isEmpty :: Board -> Bool
isEmpty (Board _ cells) = null cells

livingNeighbours :: Board -> Cell -> Int
livingNeighbours (Board _ cells) (x, y) = length $ filter id [
  (x + 1, y    ) `elem` cells,
  (x + 1, y + 1) `elem` cells,
  (x    , y + 1) `elem` cells,
  (x - 1, y + 1) `elem` cells,
  (x - 1, y    ) `elem` cells,
  (x - 1, y - 1) `elem` cells,
  (x    , y - 1) `elem` cells,
  (x + 1, y - 1) `elem` cells
  ]
