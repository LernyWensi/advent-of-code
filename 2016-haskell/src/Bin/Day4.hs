module Bin.Day4 (
    main,
    parse,
    first,
    second,
) where

import Control.Arrow ((&&&), (***))
import Data.Char.Extended (isAlpha, isDigit)
import Data.Ord (comparing)
import Data.Text.Extended (Text)
import Fail ((#?))

import Data.Char.Extended qualified as Char
import Data.List.Extended qualified as List
import Data.Text.Extended qualified as Text
import Data.Text.Read.Extended qualified as Text
import Day qualified

data Room = Room
    { name :: Text
    , sector :: Int
    , checksum :: Text
    }
    deriving stock (Show)

validateRoom :: Room -> Bool
validateRoom = uncurry (==) . (expectedChecksum . name &&& checksum)
    where
        expectedChecksum :: Text -> Text
        expectedChecksum =
            Text.pack
                . fmap Text.head
                . reverse
                . List.takeEnd checksumLength
                . List.sortBy lengthOrAlphabetization
                . Text.group
                . Text.sort
            where
                checksumLength :: Int
                checksumLength = 5

                lengthOrAlphabetization :: Text -> Text -> Ordering
                lengthOrAlphabetization = uncurry (<>) . (comparing Text.length &&& flip compare)

parse :: Text -> [Room]
parse =
    fmap
        ( uncurry asRoom
            . (extractNameAndSector *** extractChecksum)
            . Text.breakOn "["
        )
        . Text.lines
    where
        extractChecksum :: Text -> Text
        extractChecksum = Text.filter isAlpha

        extractNameAndSector :: Text -> (Text, Text)
        extractNameAndSector = fmap (Text.filter isDigit) . Text.partition isAlpha

        asRoom :: (Text, Text) -> Text -> Room
        asRoom (name, sector) checksum =
            Room
                { name = name
                , sector = Text.decimal' sector #? "Room's sector ID must be a valid decimal number; got " <> show sector
                , checksum = checksum
                }

first :: [Room] -> Maybe Int
first = Just . sum . fmap sector . filter validateRoom

second :: [Room] -> Maybe Int
second = fmap sector . List.find isNorthPoleObjectStorage . fmap decrypt . filter validateRoom
    where
        decrypt :: Room -> Room
        decrypt room@Room {..} = room {name = Text.map (Char.shift sector) name}

        isNorthPoleObjectStorage :: Room -> Bool
        isNorthPoleObjectStorage Room {..} = all (`Text.isInfixOf` name) ["object", "north", "pole"]

main :: IO ()
main = Day.solve parse first second
