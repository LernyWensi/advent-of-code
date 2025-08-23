module Day (
    solve,
) where

import Control.Arrow ((&&&))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Fail ((#?))
import Text.Printf (printf)

import Data.Text.IO qualified as Text
import System.Environment qualified as Environment

data Part = One | Two

instance Show Part where
    show One = "1"
    show Two = "2"

solvePart :: (Show solution) => Part -> (input -> Maybe solution) -> input -> IO ()
solvePart part solver input = printf
    "Part #%s: %s\n"
    (show part)
    case solver input of
        Just solution -> show solution
        Nothing -> show @String "Not Solved"

solve :: (Show solution) => (Text -> input) -> (input -> Maybe solution) -> (input -> Maybe solution) -> IO ()
solve parse first second =
    uncurry (>>) . (solvePart One first &&& solvePart Two second)
        =<< fmap parse . Text.readFile . (#? "An input file should be provider as the first argument") . listToMaybe
        =<< Environment.getArgs
