module Main (main) where

import AdventOfCode (solve, (#?))
import Data.Functor ((<&>))
import Text.Read (readMaybe)

import Day1 qualified
import System.Environment qualified as Environment

main :: IO ()
main = do
    [day, input] <-
        Environment.getArgs <&> \case
            args@[_, _] -> args
            _ -> error "You must provide `day` index and `input` file repecrily"

    case readMaybe @Int day #? "The day should be a valid decimal number between 1 and 25" of
        1 -> solve input Day1.parse (Just Day1.first) (Just Day1.second)
        day | day >= 2 && day <= 25 -> putStrLn $ "Day " <> show day <> " is not solved"
        _ -> error "The day should be a number between 1 and 25"
