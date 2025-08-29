module Fail (
    Fail (..),
) where

class (Monad monad) => Fail monad where
    infixr 1 #?
    (#?) :: monad value -> String -> value

    infixr 1 ?#
    (?#) :: String -> monad value -> value

instance Fail (Either err) where
    (#?) :: Either err value -> String -> value
    (#?) (Right value) _ = value
    (#?) (Left _) message = error message

    (?#) :: String -> Either err value -> value
    (?#) = flip (#?)

instance Fail Maybe where
    (#?) :: Maybe value -> String -> value
    (#?) (Just value) _ = value
    (#?) Nothing message = error message

    (?#) :: String -> Maybe value -> value
    (?#) = flip (#?)
