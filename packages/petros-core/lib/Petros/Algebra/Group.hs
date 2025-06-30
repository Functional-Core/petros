{-# LANGUAGE Safe #-}

module Petros.Algebra.Group
    ( Group (..)
    ) where

import Petros.Algebra.Monoid

-- Monoid with an inverse element such that a <> invert a == mempty
class Monoid a => Group a where
    invert :: a -> a
