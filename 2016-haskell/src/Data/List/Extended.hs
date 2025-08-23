module Data.List.Extended (
    module Data.List,
    chunksOf,
) where

import Data.List

chunksOf :: Int -> [item] -> [[item]]
chunksOf size = takeWhile (not . null) . unfoldr (Just . splitAt size)
