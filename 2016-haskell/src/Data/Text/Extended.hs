module Data.Text.Extended (
    module Data.Text,
    sort,
) where

import Data.Text

import Data.List qualified as List

sort :: Text -> Text
sort = pack . List.sort . unpack
