module AdventOfCode (
    Solution,
    solve,
    getInput,
    exit,
)
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.Text qualified as Text
import Data.Text.IO qualified as Text

type Solution answer error = Either answer error

solve :: (Show answer, Show error) => String -> Solution answer error -> IO ()
solve answer (Right solution) = putStrLn $ "[✔] " <> answer <> show solution
solve _ (Left failure) = putStrLn $ "[✖] " <> show failure

getInput :: IO Text
getInput = do
    input <- listToMaybe <$> getArgs
    case input of
        Just input -> Text.strip <$> Text.readFile input
        Nothing -> exit "Can't find the first argument; couldn't locate the input file path without it. :("

exit :: String -> IO exit
exit = (>> exitFailure) . putStrLn
