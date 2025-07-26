{-# LANGUAGE Safe #-}

module Petros.Algebra.Field
    ( Field
    ) where

import Petros.Algebra.EuclideanRing (EuclideanRing)
import Petros.Algebra.DivisionRing (DivisionRing)

class (DivisionRing a, EuclideanRing a) => Field a where
