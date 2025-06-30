{-# LANGUAGE Safe #-}

module Petros.Algebra.Ring
    ( Ring (..) 
    ) where

import Petros.Algebra.Semiring

-- Semiring with additive negation
-- TODO: (Semiring a, Group (Sum a)) => Ring a
class Semiring a => Ring a where
    negate :: a -> a
