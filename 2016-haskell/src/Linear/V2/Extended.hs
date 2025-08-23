module Linear.V2.Extended (
    module Linear.V2,
    manhattanFromOrigin,
    inDirectionNTimes,
    inDirection,
) where

import Data.Function (on)
import Direction (Direction)
import Linear.V2

import Direction qualified

manhattanFromOrigin :: (Num number) => V2 number -> number
manhattanFromOrigin (V2 x y) = ((+) `on` abs) x y

inDirectionNTimes :: (Num number) => Direction -> number -> V2 number -> V2 number
inDirectionNTimes direction amount = (+) (Direction.asV2 direction * pure amount)

inDirection :: (Num number) => Direction -> V2 number -> V2 number
inDirection = flip inDirectionNTimes 1
