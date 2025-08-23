module Day2Spec (
    spec,
) where

import Test.Hspec

import Bin.Day2 qualified as Day2

spec :: SpecWith ()
spec = do
    it "First Part" do
        Day2.first
            ( Day2.parse
                """
                ULL
                RRDDD
                LURDL
                UUUUD
                """
            )
            `shouldBe` Just "1985"

    it "Second Part" do
        Day2.second
            ( Day2.parse
                """
                ULL
                RRDDD
                LURDL
                UUUUD
                """
            )
            `shouldBe` Just "5DB3"
