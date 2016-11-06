module Board (Cell, Board (..), isEmpty, livingNeighbours, leftEdge, rightEdge, topEdge, bottomEdge) where

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

leftEdge :: Board -> Integer
leftEdge (Board _ cells) = minimum $ map fst cells

rightEdge :: Board -> Integer
rightEdge (Board _ cells) = maximum $ map fst cells

topEdge :: Board -> Integer
topEdge (Board _ cells) = minimum $ map snd cells

bottomEdge :: Board -> Integer
bottomEdge (Board _ cells) = maximum $ map snd cells
