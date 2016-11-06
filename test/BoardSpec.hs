module BoardSpec (main, spec) where

import Test.Hspec
import Board

main :: IO ()
main = hspec spec

emptyBoard :: Board
emptyBoard = Board 0 []

preseededBoard :: [Cell] -> Board
preseededBoard = Board 0

generation :: Board -> Integer
generation (Board generation _) = generation

cells :: Board -> [Cell]
cells (Board _ xs) = xs

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

    it "should yield an empty board, if there are two non-adjacient living cells" $ do
      isEmpty (nextGeneration $ preseededBoard [(1, 1), (3, 3)]) `shouldBe` True

    it "should handle the 'blinker' well" $ do
      cells (nextGeneration $ preseededBoard [(0, 1), (1, 1), (2, 1)]) `shouldBe` [(1, 0), (1, 1), (1,2)]

    it "increments the generation number" $ do
      generation (nextGeneration $ emptyBoard) `shouldBe` 1

  describe "#livingNeighbours" $ do
    it "should return 0 if there are no neighbours" $ do
      livingNeighbours emptyBoard (1, 1) `shouldBe` 0

    it "should find 1 living neighbour" $ do
      livingNeighbours (preseededBoard [(2, 1)]) (1, 1) `shouldBe` 1

    it "should find 2 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2)]) (1, 1) `shouldBe` 2

    it "should find 3 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2)]) (1, 1) `shouldBe` 3

    it "should find 4 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2), (0, 2)]) (1, 1) `shouldBe` 4

    it "should find 5 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1)]) (1, 1) `shouldBe` 5

    it "should find 6 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]) (1, 1) `shouldBe` 6

    it "should find 7 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0), (1, 0)]) (1, 1) `shouldBe` 7

    it "should find 8 living neighbours" $ do
      livingNeighbours (preseededBoard [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0), (1, 0), (2, 0)]) (1, 1) `shouldBe` 8

  describe "find edges" $ do
    it "should find the left edge of the board" $ do
      leftEdge (preseededBoard [(3, 3), (5, 5), (1, 7)]) `shouldBe` 1

    it "should find the right edge of the board" $ do
      rightEdge (preseededBoard [(3, 3), (5, 5), (1, 7)]) `shouldBe` 5

    it "should find the top edge of the board" $ do
      topEdge (preseededBoard [(3, 3), (5, 5), (1, 7)]) `shouldBe` 3

    it "should find the bottom edge of the board" $ do
      bottomEdge (preseededBoard [(3, 3), (5, 5), (1, 7)]) `shouldBe` 7
