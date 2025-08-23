module Bin.Day2 (
    main,
    parse,
    first,
    second,
) where

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Text (Text, pattern (:<))
import Direction (Direction)
import Linear.V2.Extended (V2 (..))

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Day qualified
import Direction qualified

traverseKeypad :: Map (V2 Int) Char -> V2 Int -> [[Direction]] -> Maybe [Char]
traverseKeypad keypad start = sequence . snd . List.mapAccumL followDirections start
    where
        followDirections :: V2 Int -> [Direction] -> (V2 Int, Maybe Char)
        followDirections position = (id &&& flip Map.lookup keypad) . foldl' step position
            where
                step :: V2 Int -> Direction -> V2 Int
                step position direction
                    | translated `Map.member` keypad = translated
                    | otherwise = position
                    where
                        translated = position + Direction.asV2 direction

parse :: Text -> [[Direction]]
parse = fmap (`parseLine` []) . Text.lines
    where
        parseLine :: Text -> [Direction] -> [Direction]
        parseLine Text.Empty = reverse
        parseLine (char :< text) = parseLine text . (Direction.fromChar char :)

first :: [[Direction]] -> Maybe String
first = traverseKeypad keypad (V2 0 0)
    where
        keypad :: Map (V2 Int) Char
        keypad = Map.fromList $ zip keys chars
            where
                chars :: [Char]
                chars = ['1' .. '9']

{- FOURMOLU_DISABLE -}
                keys :: [V2 Int]
                keys =
                    [ {- 1 -} V2 -1  1, {- 2 -} V2 0  1, {- 3 -} V2 1  1
                    , {- 4 -} V2 -1  0, {- 5 -} V2 0  0, {- 6 -} V2 1  0
                    , {- 7 -} V2 -1 -1, {- 8 -} V2 0 -1, {- 9 -} V2 1 -1
                    ]
{- FOURMOLU_ENABLE -}

second :: [[Direction]] -> Maybe String
second = traverseKeypad keypad (V2 -2 0)
    where
        keypad :: Map (V2 Int) Char
        keypad = Map.fromList $ zip keys chars
            where
                chars :: [Char]
                chars = ['1' .. '9'] <> ['A' .. 'D']

{- FOURMOLU_DISABLE -}
                keys :: [V2 Int]
                keys =
                    [                                     {- 1 -} V2 0  2
                    ,                   {- 2 -} V2 -1  1, {- 3 -} V2 0  1, {- 4 -} V2 1  1
                    , {- 5 -} V2 -2  0, {- 6 -} V2 -1  0, {- 7 -} V2 0  0, {- 8 -} V2 1  0, {- 9 -} V2 2  0
                    ,                   {- A -} V2 -1 -1, {- B -} V2 0 -1, {- C -} V2 1 -1
                    ,                                     {- D -} V2 0 -2
                    ]
{- FOURMOLU_ENABLE -}

main :: IO ()
main = Day.solve parse first second
