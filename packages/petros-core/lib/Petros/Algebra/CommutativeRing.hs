{-# LANGUAGE Safe #-}
module Petros.Algebra.CommutativeRing
    ( CommutativeRing
    ) where

import Petros.Algebra.Ring (Ring)

class (Ring a) => CommutativeRing a where
    -- a * b = b * a
