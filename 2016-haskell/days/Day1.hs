module Main (
    main,
) where

import AdventOfCode (Solution, getInput, solve)
import Control.Arrow (left, (&&&))
import Data.Function (on)
import Data.Maybe.Extra (toEither)
import Data.Text (Text)

import Data.Text qualified as Text
import Data.Text.Read qualified as Text

data Error
    = InvalidRotation Text
    | InvalidAmount Text Text
    | RotationIsNotSpecified Text

instance Show Error where
    show :: Error -> String
    show (InvalidRotation step) = "The step can only specify 'L' or 'R' for rotation; step " <> show step <> " is incorrect."
    show (InvalidAmount step amount) = "An invalid amount " <> show amount <> " has been specified for the step " <> show step <> "."
    show (RotationIsNotSpecified step) = "The step must specify the rotation; step " <> show step <> " is incorrect."

main :: IO ()
main = solve "The shortest path to the destination is " . solution =<< getInput

data Direction = North | East | South | West
    deriving (Show)

data Rotation = LeftRotation | RightRotation
    deriving (Show)

data Step = Step {rotation :: Rotation, amount :: Int}
    deriving (Show)

data Position = Position {x :: Int, y :: Int}
    deriving (Show)

solution :: Text -> Solution Error Int
solution input = do
    steps <- toSteps $ Text.splitOn ", " input
    let (Position {..}, _) = foldl' moveByStep (Position 0 0, North) steps
    Right $ ((+) `on` abs) x y

toSteps :: [Text] -> Either Error [Step]
toSteps = traverse \step -> do
    (rotation, amount) <- toEither (RotationIsNotSpecified step) $ Text.uncons step
    rotation <- toEither (InvalidRotation step) $ parseRotation rotation
    (amount, _) <- left (const $ InvalidAmount step amount) $ Text.decimal amount
    Right $ Step rotation amount
    where
        parseRotation :: Char -> Maybe Rotation
        parseRotation 'L' = Just LeftRotation
        parseRotation 'R' = Just RightRotation
        parseRotation _invalidRotation = Nothing

moveByStep :: (Position, Direction) -> Step -> (Position, Direction)
moveByStep (position, direction) step =
    move position (amount step) &&& id
        $ rotate (rotation step) direction
    where
        move :: Position -> Int -> Direction -> Position
        move position amount North = position {y = y position + amount}
        move position amount East = position {x = x position + amount}
        move position amount South = position {y = y position - amount}
        move position amount West = position {x = x position - amount}

        rotate :: Rotation -> Direction -> Direction
        rotate RightRotation North = East
        rotate RightRotation East = South
        rotate RightRotation South = West
        rotate RightRotation West = North
        rotate LeftRotation North = West
        rotate LeftRotation West = South
        rotate LeftRotation South = East
        rotate LeftRotation East = North
