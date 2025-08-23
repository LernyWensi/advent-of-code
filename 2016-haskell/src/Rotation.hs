module Rotation (
    Rotation (..),
    fromChar,
) where

import Prelude hiding (Either (..))

data Rotation = Left | Right
    deriving stock (Show)

fromChar :: Char -> Maybe Rotation
fromChar 'L' = Just Left
fromChar 'R' = Just Right
fromChar _invalidRotation = Nothing
