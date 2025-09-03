module Day6Spec (
    spec,
) where

import Bin.Day6 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first
            ( parse
                """
                eedadn
                drvtee
                eandsr
                raavrd
                atevrs
                tsrnev
                sdttsa
                rasrtv
                nssdts
                ntnada
                svetve
                tesnvt
                vntsnd
                vrdear
                dvrsen
                enarar
                """
            )
            `shouldBe` Just "easter"

    it "Second Part" do
        second
            ( parse
                """
                eedadn
                drvtee
                eandsr
                raavrd
                atevrs
                tsrnev
                sdttsa
                rasrtv
                nssdts
                ntnada
                svetve
                tesnvt
                vntsnd
                vrdear
                dvrsen
                enarar
                """
            )
            `shouldBe` Just "advent"
