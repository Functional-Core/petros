{-# LANGUAGE Safe #-}

module Petros.Algebra.CommutativeGroup
    ( CommutativeGroup
    ) where

import Petros.Algebra.Group (Group)

-- Also known as an Abelian Group
class (Group a) => CommutativeGroup a where
    -- a <> b = b <> a
