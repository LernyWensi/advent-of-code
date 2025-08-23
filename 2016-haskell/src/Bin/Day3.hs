module Bin.Day3 (
    main,
    parse,
    first,
    second,
) where

import Data.Text (Text)
import Fail ((#?))

import Data.Text qualified as Text
import Data.Text.Read.Extended qualified as Text
import Day qualified

parse :: Text -> [(Int, Int, Int)]
parse = fmap (parseTriangle . Text.splitOn "  ") . Text.lines
    where
        parseTriangle :: [Text] -> (Int, Int, Int)
        parseTriangle [a, b, c] = (,,) <$> Text.decimal' a <*> Text.decimal' b <*> Text.decimal' c #? "Triangle vertex values must be valid decimal numbers"
        parseTriangle list = error $ "Expected 3 values for a triangle; found " <> show (length list)

first :: [(Int, Int, Int)] -> Maybe Int
first _ = undefined

second :: [(Int, Int, Int)] -> Maybe Int
second _ = undefined

main :: IO ()
main = Day.solve parse first second
