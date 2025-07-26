{-# LANGUAGE Trustworthy #-}
module Petros.Algebra.DivisionRing
    ( DivisionRing (..)
    , unsafeDiv
    ) where

import Petros.Algebra.Ring (Ring)
import Petros.NonZero (NonZero (..))
import Petros.Eq (Eq(..))
import Petros.Algebra.Semiring (Semiring(..))
import Data.Bool (otherwise)
import Data.Coerce (coerce)
import GHC.Base (error)

-- Also known as a Skew Field
class (Ring a) => DivisionRing a where
    (/) :: a -> NonZero a -> a

unsafeDiv :: forall a. (DivisionRing a, Eq a) => a -> a -> a
unsafeDiv x y
    | y == zero = error "Divide by zero"
    | otherwise = x / coerce y
