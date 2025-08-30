module Day3Spec (
    spec,
) where

import Bin.Day3 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first (parse "5  10  25") `shouldBe` Just 0

    it "Second Part" do
        second
            ( parse
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
