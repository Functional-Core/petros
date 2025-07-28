{-# LANGUAGE Safe #-}

module Petros.Algebra.IntegralDomain
    ( IntegralDomain
    ) where

import Petros.Algebra.CommutativeRing (CommutativeRing)
import Petros.Algebra.Domain (Domain)

type IntegralDomain a = (CommutativeRing a, Domain a)

