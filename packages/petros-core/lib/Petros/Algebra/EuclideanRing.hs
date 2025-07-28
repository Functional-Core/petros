{-# LANGUAGE Trustworthy #-}

module Petros.Algebra.EuclideanRing
    ( EuclideanRing
    ) where

import Petros.Algebra.GcdDomain (GcdDomain)
import Petros.Algebra.EuclideanSemiring (EuclideanSemiring)

type EuclideanRing a = (GcdDomain a, EuclideanSemiring a)
