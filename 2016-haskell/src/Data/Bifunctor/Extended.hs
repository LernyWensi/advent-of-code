module Data.Bifunctor.Extended (
    module Data.Bifunctor,
    both,
) where

import Data.Bifunctor

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f
