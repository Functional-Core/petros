{-# LANGUAGE Safe #-}
module Petros.Algebra.Domain
    ( Domain 
    ) where

import Petros.Algebra.Ring (Ring)

class (Ring a) => Domain a where
    -- if a != 0 && b != 0 then a * b != 0
