module Day3Spec (
    spec,
) where

import Test.Hspec

import Bin.Day3 qualified as Day3

spec :: SpecWith ()
spec = do
    it "First Part" do
        Day3.first
            ( Day3.parse
                """
                5  10  25
                """
            )
            `shouldBe` Just 0

    it "Second Part" do
        Day3.second
            ( Day3.parse
                """
                101  301  501
                102  302  502
                103  303  503
                201  401  601
                202  402  602
                203  403  603
                """
            )
            `shouldBe` Just 6
