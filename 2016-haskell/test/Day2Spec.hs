module Day2Spec (
    spec,
) where

import Bin.Day2 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first
            ( parse
                """
                ULL
                RRDDD
                LURDL
                UUUUD
                """
            )
            `shouldBe` Just "1985"

    it "Second Part" do
        second
            ( parse
                """
                ULL
                RRDDD
                LURDL
                UUUUD
                """
            )
            `shouldBe` Just "5DB3"
