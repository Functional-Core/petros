{-# LANGUAGE Trustworthy #-}

module Petros.Algebra.EuclideanRing
    ( EuclideanRing (..)
    , (%)
    ) where

import GHC.Natural (Natural)
import Petros.Algebra.GcdDomain (GcdDomain)
import Petros.NonZero (NonZero (..))
import Data.Coerce (coerce)

class (GcdDomain a) => EuclideanRing a where
    quot :: a -> NonZero a -> a
    rem :: a -> NonZero a -> a
    quotRem :: a -> a -> (a, a)
    degree :: a -> Natural

(%) :: forall a. EuclideanRing a => a -> a -> a
x % y = rem x (coerce y)
{-# INLINE (%) #-}
