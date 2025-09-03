module Bin.Day6 (
    main,
    parse,
    first,
    second,
) where

import Data.Text (Text)

import Data.List qualified as List
import Data.Text.Extended qualified as Text
import Day qualified

parse :: Text -> [Text]
parse = Text.transpose . Text.splitOn "\n"

decode :: ([Text] -> Text) -> [Text] -> Text
decode extractor = Text.pack . fmap (Text.head . extractor . List.sortOn Text.length . Text.group . Text.sort)

first :: [Text] -> Maybe Text
first = Just . decode last

second :: [Text] -> Maybe Text
second = Just . decode head

main :: IO ()
main = Day.solve parse first second
