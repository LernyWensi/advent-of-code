module Day1 (parse, first, second) where

import AdventOfCode ((#?))
import Control.Monad (foldM)
import Data.Function (on)
import Data.Set (Set)
import Data.Text (Text)

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Read qualified as Text

data Direction = North | East | South | West
    deriving stock (Enum, Bounded, Show)

data Rotation = L | R
    deriving stock (Show)

data Instruction = Instruction {rotation :: Rotation, amount :: Int}
    deriving stock (Show)

data Position = Position {x :: Int, y :: Int}
    deriving stock (Eq, Ord, Show)

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

rotate :: Direction -> Rotation -> Direction
rotate (fromEnum -> direction) =
    toEnum . abs . \case
        R -> (direction + 1) `mod` bound
        L -> (direction - 1) `mod` bound
    where
        bound :: Int
        bound = fromEnum (maxBound @Direction) + 1

transpose :: Direction -> Int -> Position -> Position
transpose North amount position@(Position _ y) = position {y = y + amount}
transpose East amount position@(Position x _) = position {x = x + amount}
transpose South amount position@(Position _ y) = position {y = y - amount}
transpose West amount position@(Position x _) = position {x = x - amount}

startingPoint :: (Position, Direction)
startingPoint = (Position 0 0, North)

distanceFromStartingPoint :: Position -> Int
distanceFromStartingPoint Position {..} = ((+) `on` abs) x y

first :: [Instruction] -> Int
first =
    distanceFromStartingPoint
        . fst
        . foldl' stepByInstruction startingPoint
    where
        stepByInstruction :: (Position, Direction) -> Instruction -> (Position, Direction)
        stepByInstruction (position, direction) Instruction {..} = (newPosition, newDirection)
            where
                newDirection = rotate direction rotation
                newPosition = transpose newDirection amount position

second :: [Instruction] -> Int
second =
    distanceFromStartingPoint
        . either id (fst . fst)
        . foldM stepTrackingPath (startingPoint, Set.empty)
    where
        stepTrackingPath :: ((Position, Direction), Set Position) -> Instruction -> Either Position ((Position, Direction), Set Position)
        stepTrackingPath ((position, direction), visited) Instruction {..}
            | not $ Set.null pathIntersections = Left $ Set.elemAt 0 pathIntersections
            | otherwise = Right ((transpose newDirection amount position, newDirection), Set.union visited pathToPosition)
            where
                newDirection :: Direction
                newDirection = rotate direction rotation

                pathToPosition :: Set Position
                pathToPosition = Set.fromList . take amount $ iterate (transpose newDirection 1) position

                pathIntersections :: Set Position
                pathIntersections = Set.intersection pathToPosition visited
