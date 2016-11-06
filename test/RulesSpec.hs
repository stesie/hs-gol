module RulesSpec (main, spec) where

import Test.Hspec
import Rules (isAlive)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "#isAlive" $ do
    describe "alive cells die of starvation or over-population" $ do
      it "should die if it has zero neighbours" $ do
        isAlive True 0 `shouldBe` False

      it "should die if it has five neighbours" $ do
        isAlive True 5 `shouldBe` False

    describe "cells with two neighbours" $ do
      it "should stay dead, if it is already dead" $ do
        isAlive False 2 `shouldBe` False

      it "should stay alive, if it is alive" $ do
        isAlive True 2 `shouldBe` True

    describe "cells with three neighbours" $ do
      it "should resurrect, if it is currently dead" $ do
        isAlive False 3 `shouldBe` True

      it "should stay alive, if it is alive" $ do
        isAlive True 3 `shouldBe` True
