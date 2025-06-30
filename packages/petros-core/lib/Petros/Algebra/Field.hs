{-# LANGUAGE Safe #-}

module Petros.Algebra.Field
    ( Field (..)
    ) where

import Petros.Algebra.Ring

-- TODO: (/) :: a -> NonZero a -> a

-- TODO: (Ring a, Group (Prod a)) => Field a
class Ring a => Field a where
    (/) :: a -> a -> a
