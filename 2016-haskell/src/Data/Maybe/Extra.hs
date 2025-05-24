module Data.Maybe.Extra (
    toEither,
) where

toEither :: error -> Maybe value -> Either error value
toEither _ (Just value) = Right value
toEither error Nothing = Left error
