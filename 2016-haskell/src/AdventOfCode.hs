module AdventOfCode (solve, (#?)) where

import Data.Text (Text)

import Data.Text.IO qualified as Text

class (Monad monad) => Fail monad where
    infixr 1 #?
    (#?) :: monad value -> String -> value

instance Fail (Either err) where
    (#?) :: Either err value -> String -> value
    (#?) (Right value) _ = value
    (#?) (Left _) message = error message

instance Fail Maybe where
    (#?) :: Maybe value -> String -> value
    (#?) (Just value) _ = value
    (#?) Nothing message = error message

solve
    :: (Show solution)
    => FilePath -> (Text -> input) -> Maybe (input -> solution) -> Maybe (input -> solution) -> IO ()
solve input parse first second = do
    input <- parse <$> Text.readFile input
    putStrLn $ "Part #1: " <> maybe "Not solved" (show . ($ input)) first
    putStrLn $ "Part #2: " <> maybe "Not solved" (show . ($ input)) second
