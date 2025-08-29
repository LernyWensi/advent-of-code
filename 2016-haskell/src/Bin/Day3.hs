module Bin.Day3 (
    main,
    parse,
    first,
    second,
) where

import Data.Text (Text)
import Fail (Fail (..))

import Data.List.Extended qualified as List
import Data.Text qualified as Text
import Data.Text.Read.Extended qualified as Text
import Day qualified

countPossibleTriangles :: [(Int, Int, Int)] -> Int
countPossibleTriangles = length . List.filter isTrianglePossible
    where
        isTrianglePossible :: (Int, Int, Int) -> Bool
        isTrianglePossible (a, b, c) = a + b > c && a + c > b && b + c > a

asTriangle :: (Show a) => [a] -> (a, a, a)
asTriangle [a, b, c] = (a, b, c)
asTriangle chunk = error $ "Expected 3 values for a triangle; got " <> show chunk

parse :: Text -> [(Int, Int, Int)]
parse =
    fmap
        ( parseTriangle
            . fmap Text.strip
            . filter (/= Text.empty)
            . Text.splitOn "  "
        )
        . Text.lines
    where
        parseTriangle :: [Text] -> (Int, Int, Int)
        parseTriangle list = asTriangle $ traverse Text.decimal' list #? "Triangle vertex values must be valid decimal numbers; got " <> show list

first :: [(Int, Int, Int)] -> Maybe Int
first = Just . countPossibleTriangles

second :: [(Int, Int, Int)] -> Maybe Int
second = Just . (\(a, b, c) -> count a + count b + count c) . unzip3
    where
        count :: [Int] -> Int
        count = countPossibleTriangles . fmap asTriangle . List.chunksOf 3

main :: IO ()
main = Day.solve parse first second
