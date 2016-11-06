module BoardSpec (main, spec) where

import Test.Hspec
import Rules (isAlive)

main :: IO ()
main = hspec spec

type Cell = (Integer, Integer)

data Board = Board Integer [Cell]
  deriving (Eq, Show)

emptyBoard :: Board
emptyBoard = Board 0 []

preseededBoard :: [Cell] -> Board
preseededBoard = Board 0

isEmpty :: Board -> Bool
isEmpty (Board _ cells) = null cells

nextGeneration :: Board -> Board
nextGeneration (Board generation cells) = Board (succ generation) (nextGeneration' cells)

nextGeneration' :: [Cell] -> [Cell]
nextGeneration' [] = []
nextGeneration' (x:xs)
  | isAlive True 0 == False = nextGeneration' xs

spec :: Spec
spec = do
  describe "#isEmpty" $ do
    it "should be true on empty board" $ do
      isEmpty emptyBoard `shouldBe` True

    it "should be false on preseeded, non-empty board" $ do
      isEmpty (preseededBoard [(1, 3), (3, 1)]) `shouldBe` False

  describe "#nextGeneration" $ do
    it "should yield an empty board, if there is only one alive cell" $ do
      isEmpty (nextGeneration $ preseededBoard [(1, 1)]) `shouldBe` True
