{-# LANGUAGE Safe #-}

module Petros.Algebra.GcdDomain
    ( GcdDomain (..)
    ) where

import Petros.Algebra.IntegralDomain (IntegralDomain)

class (IntegralDomain a) => GcdDomain a where
    gcd :: a -> a -> a
