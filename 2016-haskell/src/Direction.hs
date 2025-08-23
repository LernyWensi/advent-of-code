module Direction (
    Direction (..),
    fromCharMaybe,
    fromChar,
    asV2,
    rotate,
) where

import Data.Map (Map)
import Linear (V2 (..))
import Rotation (Rotation)
import Prelude hiding (Either (..))

import Data.List qualified as List
import Data.Map qualified as Map
import Rotation qualified

data Direction = Up | Right | Down | Left
    deriving stock (Enum, Bounded, Show)

{- FOURMOLU_DISABLE -}
directions :: Map Char Direction
directions =
    Map.fromList
        [ ('U', Up), ('R', Right), ('D', Down), ('L', Left)
        , ('N', Up), ('E', Right), ('S', Down), ('W', Left)
        ]
{- FOURMOLU_ENABLE -}

aliases :: String
aliases = List.intercalate ", " $ pure <$> Map.keys directions

fromCharMaybe :: Char -> Maybe Direction
fromCharMaybe = flip Map.lookup directions

fromChar :: Char -> Direction
fromChar char = case fromCharMaybe char of
    Just direction -> direction
    _ -> error $ "Received an invalid direction: " <> show char <> "; try any of: " <> aliases

{- FOURMOLU_DISABLE -}
asV2 :: (Num number) => Direction -> V2 number
asV2 Up    = V2  0  1
asV2 Right = V2 -1  0
asV2 Down  = V2  0 -1
asV2 Left  = V2  1  0
{- FOURMOLU_ENABLE -}

rotate :: Direction -> Rotation -> Direction
rotate (fromEnum -> direction) =
    toEnum . abs . \case
        Rotation.Right -> (direction + 1) `mod` bound
        Rotation.Left -> (direction - 1) `mod` bound
    where
        bound :: Int
        bound = fromEnum (maxBound @Direction) + 1
