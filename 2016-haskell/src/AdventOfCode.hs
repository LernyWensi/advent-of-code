module AdventOfCode (solve, (#?), (#!)) where

import Data.Text (Text)

import Data.Text.IO qualified as Text

solve
    :: (Show solution)
    => FilePath -> (Text -> input) -> Maybe (input -> solution) -> Maybe (input -> solution) -> IO ()
solve input parse first second = do
    input <- parse <$> Text.readFile input
    putStrLn $ "Part #1: " <> maybe "Not solved" (show . ($ input)) first
    putStrLn $ "Part #2: " <> maybe "Not solved" (show . ($ input)) second

infixr 1 #?
(#?) :: Maybe m -> String -> m
(#?) (Just m) _ = m
(#?) Nothing message = error message

infixr 1 #!
(#!) :: Either l r -> String -> r
(#!) (Right r) _ = r
(#!) (Left _) message = error message
