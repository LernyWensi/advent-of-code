module Bin.Day5 (
    main,
    parse,
    first,
    second,
) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Digest.Pure.MD5 (MD5Context)
import Data.Text (Text, pattern (:<))

import Data.Char.Extended qualified as Char
import Data.Digest.Pure.MD5 qualified as MD5
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Day qualified

pattern PasswordChar :: Char -> Text
pattern PasswordChar char <-
    '0' :< '0' :< '0' :< '0' :< '0'
        :< char
        :< _remainingHash

pattern PasswordPositionChar :: Int -> Char -> Text
pattern PasswordPositionChar position char <-
    '0' :< '0' :< '0' :< '0' :< '0'
        :< (Char.maybeDigitToInt -> Just position)
        :< char
        :< _remainingHash

parse :: Text -> (Int -> Text)
parse input = Text.show . MD5.hash' @MD5Context . Text.encodeUtf8 . (Text.strip input <>) . Text.show

first :: (Int -> Text) -> Maybe Text
first hash = Just $ go Text.empty 0
    where
        go :: Text -> Int -> Text
        go password _ | Text.length password == 8 = password
        go password ((+ 1) &&& hash -> (index, PasswordChar char)) = go (password <> Text.singleton char) index
        go password ((+ 1) -> index) = go password index

second :: (Int -> Text) -> Maybe Text
second hash = Just $ go (Seq.replicate 8 Nothing) 0
    where
        go (fmap (reverse . foldr (:) []) . sequence -> Just password) _ = Text.reverse . Text.pack $ password
        go password ((+ 1) &&& hash -> (index, PasswordPositionChar position char)) = go (Seq.adjust' (<|> Just char) position password) index
        go password ((+ 1) -> index) = go password index

main :: IO ()
main = Day.solve parse first second
