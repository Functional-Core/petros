{-# LANGUAGE Safe #-}

module Petros.Algebra.Semiring
    ( Semiring (..)
    ) where

-- TODO: class (Monoid (Sum a), Monoid (Prod a)) => Semiring a where...
-- Types with both an additive and multiplicative monoid.
class Semiring a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
