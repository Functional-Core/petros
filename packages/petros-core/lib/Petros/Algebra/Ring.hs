{-# LANGUAGE Safe #-}

module Petros.Algebra.Ring
    ( Ring (..) 
    ) where

import Petros.Algebra.Semiring
import Petros.Algebra.Group
import Petros.Algebra.Monoid
import Data.Function ((.))

-- Semiring with additive negation
class (Semiring a, Group (Sum a)) => Ring a where
    negate :: a -> a
    negate = getSum . inverse . Sum
    {-# INLINE negate #-}

    (-) :: a -> a -> a
    x - y = x + (negate y)
