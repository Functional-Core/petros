{-# LANGUAGE Safe #-}

module Petros.Algebra.Semiring
    ( Semiring (..)
    ) where


import Petros.Algebra.Monoid
import Petros.Algebra.Semigroup

-- TODO: class (Monoid (Sum a), Monoid (Prod a)) => Semiring a where...
-- Types with both an additive and multiplicative monoid.
class (Monoid (Sum a), Monoid (Product a)) => Semiring a where
    zero :: a
    zero = getSum mempty
    {-# INLINE zero #-}

    one :: a
    one = getProduct mempty
    {-# INLINE one #-}

    (+) :: a -> a -> a
    x + y = getSum (Sum x <> Sum y)
    {-# INLINE (+) #-}

    (*) :: a -> a -> a
    x * y = getProduct (Product x <> Product y)
    {-# INLINE (*) #-}
