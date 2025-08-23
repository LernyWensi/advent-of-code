module Day3Spec (
    spec,
) where

import Test.Hspec

import Bin.Day3 qualified as Day3

spec :: SpecWith ()
spec = do
    it "First Part" do
        Day3.first (Day3.parse "5  10  25") `shouldBe` Just 0
