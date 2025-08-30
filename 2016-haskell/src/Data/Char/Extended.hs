module Data.Char.Extended (
    module Data.Char,
    shift,
) where

import Data.Char

shift :: Int -> Char -> Char
shift amount char
    | isAsciiLower char = chr . shift $ ord 'a'
    | isAsciiUpper char = chr . shift $ ord 'A'
    | otherwise = char
    where
        shift :: Int -> Int
        shift base = base + ((ord char - base + amount) `mod` alphabetSize)
            where
                alphabetSize :: Int
                alphabetSize = 26
