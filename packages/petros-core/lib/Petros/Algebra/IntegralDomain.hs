{-# LANGUAGE Safe #-}

module Petros.Algebra.IntegralDomain
    ( IntegralDomain
    ) where

import Petros.Algebra.CommutativeRing (CommutativeRing)

class (CommutativeRing a) => IntegralDomain a where
