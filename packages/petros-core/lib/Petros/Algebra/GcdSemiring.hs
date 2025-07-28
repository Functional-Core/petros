{-# LANGUAGE Safe #-}

module Petros.Algebra.GcdSemiring
    ( GcdSemiring (..)
    ) where
import Petros.Algebra.Semiring (Semiring)

class (Semiring a) => GcdSemiring a where
    gcd :: a -> a -> a
