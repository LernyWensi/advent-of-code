module Day1Spec (
    spec,
) where

import Bin.Day1 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first (parse "R2, L3") `shouldBe` Just 5
        first (parse "R2, R2, R2") `shouldBe` Just 2
        first (parse "R5, L5, R5, R3") `shouldBe` Just 12

    it "Second Part" do
        second (parse "R8, R4, R4, R8") `shouldBe` Just 4
