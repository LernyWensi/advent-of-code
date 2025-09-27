module Day7Spec (
    spec,
) where

import Bin.Day7 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first
            ( parse
                """
                abba[mnop]qrst
                abcd[bddb]xyyx
                aaaa[qwer]tyui
                ioxxoj[asdfgh]zxcvbn
                """
            )
            `shouldBe` Just 2

    it "Second Part" do
        second
            ( parse
                """
                aba[bab]xyz
                xyx[xyx]xyx
                aaa[kek]eke
                zazbz[bzb]cdb
                """
            )
            `shouldBe` Just 3
