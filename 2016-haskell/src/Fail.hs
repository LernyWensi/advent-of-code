module Fail (
    Fail (..),
) where

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
