module Bin.Day7 (
    main,
    parse,
    first,
    second,
) where

import Control.Arrow ((&&&))
import Data.Bifunctor.Extended (both)
import Data.Text (Text, pattern (:<))

import Data.Text qualified as Text
import Day qualified

data Address = Address
    { hypernet :: [Text]
    , supernet :: [Text]
    }
    deriving stock (Show)

extendHypernet :: Address -> Text -> Address
extendHypernet address@Address {hypernet} hypernet' = address {hypernet = hypernet' : hypernet}

extendSupernet :: Address -> Text -> Address
extendSupernet address@Address {supernet} supernet' = address {supernet = supernet' : supernet}

parse :: Text -> [Address]
parse = fmap (parseAddress $ Address [] []) . Text.lines
    where
{- FOURMOLU_DISABLE -}
        parseAddress :: Address -> Text -> Address
        parseAddress address Text.Empty = address
        parseAddress (extendHypernet -> extend) ('[' :< (Text.breakOn "]" -> (hypernet, rest))) = parseAddress (extend hypernet) rest
        parseAddress (extendSupernet -> extend) (']' :< (Text.breakOn "[" -> (supernet, rest))) = parseAddress (extend supernet) rest
        parseAddress (extendSupernet -> extend) (       (Text.breakOn "[" -> (supernet, rest))) = parseAddress (extend supernet) rest
{- FOURMOLU_ENABLE -}

first :: [Address] -> Maybe Int
first = Just . length . filter (uncurry (&&) . (not . isABBApresent'Hypernet &&& isABBAPresent'Supernet))
    where
        isABBApresent'Hypernet :: Address -> Bool
        isABBApresent'Hypernet = any isABBAPresent . hypernet

        isABBAPresent'Supernet :: Address -> Bool
        isABBAPresent'Supernet = any isABBAPresent . supernet

        isABBAPresent :: Text -> Bool
        isABBAPresent = any isValidABBA . intoABBAs []
            where
                isValidABBA :: (Char, Char, Char, Char) -> Bool
                isValidABBA (a, b, b', a') = a == a' && a /= b && b == b'

                intoABBAs :: [(Char, Char, Char, Char)] -> Text -> [(Char, Char, Char, Char)]
                intoABBAs abbas (a :< b :< b' :< a' :< rest) = intoABBAs ((a, b, b', a') : abbas) $ b :< b' :< a' :< rest
                intoABBAs abbas _ = abbas

second :: [Address] -> Maybe Int
second = Just . length . filter (uncurry isABA'BABPairPresent . intoABAs'BABs)
    where
        intoABAs'BABs :: Address -> ([(Char, Char, Char)], [(Char, Char, Char)])
        intoABAs'BABs = both (concatMap (intoABAs [])) . (hypernet &&& supernet)
            where
                -- Transforming to ABA is the same as transforming to BAB, so the function is the same.
                intoABAs :: [(Char, Char, Char)] -> Text -> [(Char, Char, Char)]
                intoABAs abas (a :< b :< a' :< rest) = intoABAs ((a, b, a') : abas) $ b :< a' :< rest
                intoABAs abas _ = abas

        isABA'BABPairPresent :: [(Char, Char, Char)] -> [(Char, Char, Char)] -> Bool
        isABA'BABPairPresent abas babs = any (\aba -> any (isValidABA'BAB aba) babs) abas
            where
                isValidABA'BAB :: (Char, Char, Char) -> (Char, Char, Char) -> Bool
                isValidABA'BAB (aba'a1, aba'b, aba'a2) (bab'b1, bab'a, bab'b2) = isValidABA && isValidBAB
                    where
                        isValidABA :: Bool
                        isValidABA =
                            aba'a1 == aba'a2
                                && aba'a1 /= aba'b

                        isValidBAB :: Bool
                        isValidBAB =
                            aba'a1 == bab'a
                                && aba'b == bab'b1
                                && aba'b == bab'b2

main :: IO ()
main = Day.solve parse first second
