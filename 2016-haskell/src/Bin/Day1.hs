module Bin.Day1 (
    main,
    parse,
    first,
    second,
) where

import Control.Monad (foldM)
import Data.Foldable (find)
import Data.Set (Set)
import Data.Text (Text)
import Direction (Direction)
import Fail ((#?))
import Linear.V2.Extended (V2 (..))
import Rotation (Rotation)

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Day qualified
import Direction qualified
import Linear.V2.Extended qualified as V2
import Rotation qualified

data Instruction = Instruction
    { rotation :: Rotation
    , amount :: Int
    }
    deriving stock (Show)

initialState :: (V2 Int, Direction)
initialState = (V2 0 0, Direction.Up)

parse :: Text -> [Instruction]
parse = fmap parseInstruction . Text.splitOn ", "
    where
        parseInstruction :: Text -> Instruction
        parseInstruction instruction = Instruction rotation' amount'
            where
                (rotation, amount) = Text.uncons instruction #? "The instruction must specify the rotation; instruction " <> show instruction <> " is incorrect."
                rotation' = Rotation.fromChar rotation #? "The instruction can only specify 'L' or 'R' for rotation; instruction " <> show instruction <> " is incorrect."
                amount' = fst $ Text.decimal amount #? "An invalid amount " <> show amount <> " has been specified for the instruction " <> show instruction <> "."

first :: [Instruction] -> Maybe Int
first =
    Just
        . V2.manhattanFromOrigin
        . fst
        . foldl' stepByInstruction initialState
    where
        stepByInstruction :: (V2 Int, Direction) -> Instruction -> (V2 Int, Direction)
        stepByInstruction (position, direction) Instruction {..} = (newPosition, newDirection)
            where
                newDirection = Direction.rotate direction rotation
                newPosition = V2.inDirectionNTimes newDirection amount position

second :: [Instruction] -> Maybe Int
second =
    Just
        . V2.manhattanFromOrigin
        . either id (fst . fst)
        . foldM stepTrackingPath (initialState, Set.empty)
    where
        stepTrackingPath
            :: ((V2 Int, Direction), Set (V2 Int))
            -> Instruction
            -> Either (V2 Int) ((V2 Int, Direction), Set (V2 Int))
        stepTrackingPath ((positon, direction), visited) Instruction {..}
            | Just intersection <- find (`Set.member` visited) path = Left intersection
            | otherwise = Right ((newPosition, newDirection), Set.union visited $ Set.fromList path)
            where
                newDirection :: Direction
                newDirection = Direction.rotate direction rotation

                newPosition :: (V2 Int)
                newPosition
                    | amount == 0 = positon
                    | otherwise = last (positon : path)

                path :: [V2 Int]
                path = take amount . drop 1 $ iterate (V2.inDirection newDirection) positon

main :: IO ()
main = Day.solve parse first second
