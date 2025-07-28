{-# LANGUAGE Safe #-}

module Petros.Algebra.Field
    ( Field
    ) where

import Petros.Algebra.EuclideanRing (EuclideanRing)
import Petros.Algebra.DivisionRing (DivisionRing)

type Field a = (DivisionRing a, EuclideanRing a)
