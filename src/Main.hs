module Main where

import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Random
import Data.List (intercalate, nub)
import Data.List.Split (chunksOf)

import Board

main :: IO ()
main = do
  gen <- getStdGen
  let initialCells = take 100 . nub . map (\[x, y] -> (x, y)) . chunksOf 2 $ randomRs (0, 40) gen
  mapM_ visualizeBoard (evolve $ Board 0 initialCells)

visualizeBoard :: Board -> IO ()
visualizeBoard board = do
  clearScreen
  setCursorPosition 0 0
  print board
  threadDelay 250000

instance Show Board where
  show board@(Board generation cells) =
    "Generation " ++ show generation ++ "\n"
     ++ intercalate "\n" (map (showLine board) [(topEdge board)..(bottomEdge board)])
    where showLine board@(Board _ cells) y = map (\x -> if (x, y) `elem` cells then 'x' else ' ') [(leftEdge board)..(rightEdge board)]

evolve :: Board -> [Board]
evolve board = board' : evolve board'
  where board' = nextGeneration board
