module Board (Cell, Board (..), isEmpty, livingNeighbours, leftEdge, rightEdge, topEdge, bottomEdge, nextGeneration) where

import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Rules (isAlive)

type Cell = (Integer, Integer)

data Board = Board Integer [Cell]
--  deriving (Eq, Show)

-- FIXME add tests
instance Show Board where
  show board@(Board generation cells) =
    "Generation " ++ show generation ++ "\n"
     ++ intercalate "\n" (map (showLine board) [(topEdge board)..(bottomEdge board)])
    where showLine board@(Board _ cells) y = map (\x -> if (x, y) `elem` cells then 'x' else ' ') [(leftEdge board)..(rightEdge board)]


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

nextGeneration :: Board -> Board
nextGeneration board@(Board generation cells) =
  Board (succ generation) (mapMaybe checkCell cellsToCheck)
  where
    checkCell cell = if isAlive (cell `elem` cells) (livingNeighbours board cell) then Just cell else Nothing
    cellsToCheck = [(x, y) | x <- [(leftEdge board - 1)..(rightEdge board + 1)], y <- [(topEdge board - 1)..(bottomEdge board + 1)]]
