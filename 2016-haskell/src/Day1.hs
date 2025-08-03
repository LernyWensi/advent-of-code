module Day1 (parse, first) where

import AdventOfCode ((#?))
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Text (Text)

import Data.Text qualified as Text
import Data.Text.Read qualified as Text

data Direction = North | East | South | West
    deriving stock (Enum, Bounded, Show)

data Rotation = L | R
    deriving stock (Show)

data Instruction = Instruction {rotation :: Rotation, amount :: Int}
    deriving stock (Show)

data Position = Position {x :: Int, y :: Int}
    deriving stock (Show)

parse :: Text -> [Instruction]
parse = fmap parseInstruction . Text.splitOn ", "
    where
        parseInstruction :: Text -> Instruction
        parseInstruction instruction = do
            let (rotation, amount) = Text.uncons instruction #? "The instruction must specify the rotation; instruction " <> show instruction <> " is incorrect."
                rotation' = parseRotation rotation #? "The instruction can only specify 'L' or 'R' for rotation; instruction " <> show instruction <> " is incorrect."
                (amount', _) = Text.decimal amount #? "An invalid amount " <> show amount <> " has been specified for the instruction " <> show instruction <> "."
             in Instruction rotation' amount'
            where
                parseRotation :: Char -> Maybe Rotation
                parseRotation 'L' = Just L
                parseRotation 'R' = Just R
                parseRotation _invalidRotation = Nothing

first :: [Instruction] -> Int
first =
    (\(Position {..}) -> ((+) `on` abs) x y)
        . fst
        . foldl' moveByInstruction (Position 0 0, North)
    where
        moveByInstruction :: (Position, Direction) -> Instruction -> (Position, Direction)
        moveByInstruction (position, direction) instruction =
            move position (amount instruction) &&& id
                $ rotate direction (rotation instruction)
            where
                move :: Position -> Int -> Direction -> Position
                move position@(Position _ y) amount North = position {y = y + amount}
                move position@(Position x _) amount East = position {x = x + amount}
                move position@(Position _ y) amount South = position {y = y - amount}
                move position@(Position x _) amount West = position {x = x - amount}

                rotate :: Direction -> Rotation -> Direction
                rotate (fromEnum -> direction) =
                    toEnum . abs . \case
                        R -> (direction + 1) `mod` bound
                        L -> (direction - 1) `mod` bound
                    where
                        bound :: Int
                        bound = fromEnum (maxBound @Direction) + 1
