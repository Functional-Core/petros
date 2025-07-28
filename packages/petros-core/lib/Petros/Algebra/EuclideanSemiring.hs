{-# LANGUAGE Trustworthy #-}

module Petros.Algebra.EuclideanSemiring
    ( EuclideanSemiring (..)
    , (%)
    ) where

import Petros.Algebra.GcdSemiring (GcdSemiring)
import GHC.Natural (Natural)
import Petros.NonZero (NonZero (..))
import Data.Coerce (coerce)

class (GcdSemiring a) => EuclideanSemiring a where
    quot :: a -> NonZero a -> a
    rem :: a -> NonZero a -> a
    quotRem :: a -> a -> (a, a)
    degree :: a -> Natural

(%) :: forall a. EuclideanSemiring a => a -> a -> a
x % y = rem x (coerce y)
{-# INLINE (%) #-}
