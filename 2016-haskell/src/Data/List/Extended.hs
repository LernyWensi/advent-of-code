module Data.List.Extended (
    module Data.List,
    chunksOf,
    takeEnd,
) where

import Data.List

chunksOf :: Int -> [item] -> [[item]]
chunksOf chunkSize = takeWhile (not . null) . unfoldr (Just . splitAt chunkSize)

takeEnd :: Int -> [a] -> [a]
takeEnd amount list = drop (length list - amount) list
