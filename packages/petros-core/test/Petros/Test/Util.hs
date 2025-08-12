{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Test.Util where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import Petros.Algebra.Semigroup (Product (..), Sum (..))

deriving newtype instance GenValid a => GenValid (Sum a)
deriving newtype instance Validity a => Validity (Sum a)

deriving newtype instance GenValid a => GenValid (Product a)
deriving newtype instance Validity a => Validity (Product a)


