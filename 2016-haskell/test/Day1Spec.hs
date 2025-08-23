module Day1Spec (
    spec,
) where

import Test.Hspec

import Bin.Day1 qualified as Day1

spec :: SpecWith ()
spec = do
    it "First Part" do
        Day1.first (Day1.parse "R2, L3") `shouldBe` Just 5
        Day1.first (Day1.parse "R2, R2, R2") `shouldBe` Just 2
        Day1.first (Day1.parse "R5, L5, R5, R3") `shouldBe` Just 12

    it "Second Part" do
        Day1.second (Day1.parse "R8, R4, R4, R8") `shouldBe` Just 4
