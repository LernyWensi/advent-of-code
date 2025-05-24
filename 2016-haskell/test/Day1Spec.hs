module Day1Spec (spec) where

import Day1 (first, parse)
import Test.Hspec

spec :: SpecWith ()
spec = do
    it "First Part" do
        first (parse "R2, L3") `shouldBe` 5
        first (parse "R2, R2, R2") `shouldBe` 2
        first (parse "R5, L5, R5, R3") `shouldBe` 12
