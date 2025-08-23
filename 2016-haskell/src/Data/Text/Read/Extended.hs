module Data.Text.Read.Extended (
    module Data.Text.Read,
    decimal',
) where

import Data.Text (Text)
import Data.Text.Read

decimal' :: Text -> Either String Int
decimal' = fmap fst . decimal
