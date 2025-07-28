{-# LANGUAGE Safe #-}

module Petros.Algebra.GcdDomain
    ( GcdDomain
    ) where

import Petros.Algebra.IntegralDomain (IntegralDomain)
import Petros.Algebra.GcdSemiring (GcdSemiring)

type GcdDomain a = (IntegralDomain a, GcdSemiring a)
