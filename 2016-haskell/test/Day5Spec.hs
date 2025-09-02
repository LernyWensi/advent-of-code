module Day5Spec (
    spec,
) where

import Bin.Day5 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first (parse "abc") `shouldBe` Just "18f47a30"

    it "Second Part" do
        second (parse "abc") `shouldBe` Just "05ace8e3"
