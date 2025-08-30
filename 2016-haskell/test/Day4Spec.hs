module Day4Spec (
    spec,
) where

import Bin.Day4 (first, parse, second)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = do
    it "First Part" do
        first (parse "aaaaa-bbb-z-y-x-123[abxyz]") `shouldBe` Just 123
        first (parse "a-b-c-d-e-f-g-h-987[abcde]") `shouldBe` Just 987
        first (parse "not-a-real-room-404[oarel]") `shouldBe` Just 404
        first (parse "totally-real-room-200[decoy]") `shouldBe` Just 0

        first
            ( parse
                """
                aaaaa-bbb-z-y-x-123[abxyz]
                a-b-c-d-e-f-g-h-987[abcde]
                not-a-real-room-404[oarel]
                totally-real-room-200[decoy]
                """
            )
            `shouldBe` Just 1514

    it "Second Part" do
        second
            ( parse
                """
                aaaaa-bbb-z-y-x-123[abxyz]
                a-b-c-d-e-f-g-h-987[abcde]
                bcfhvdczs-cpxsqh-ghcfous-324[chsfb]
                totally-real-room-200[decoy]
                """
            )
            `shouldBe` Just 324
