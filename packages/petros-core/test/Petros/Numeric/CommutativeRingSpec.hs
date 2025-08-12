{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Numeric.CommutativeRingSpec (spec) where

import Data.GenValidity
import Petros.Eq.Eq
import Petros.Eq.PartialEq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.Validity.Property
import Prelude hiding (Eq (..), Num (..))
import Prelude qualified
import Petros.Algebra.Semigroup (Sum (..), Product (..))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Float (Double, Float)
import GHC.Natural (Natural)
import GHC.Real
    ( Rational
    , Ratio (..)
    )
import Data.Fixed (Fixed (..), Centi)
import Petros.Test.Util ()
import Petros.Algebra.CommutativeRing (CommutativeRing)
import Petros.Algebra.Semiring (Semiring(..))

spec :: Spec
spec = describe "Commutative Ring" do
    justBase @Int "Int"
    justBase @Int8 "Int8"
    justBase @Int16 "Int16"
    justBase @Int32 "Int32"
    justBase @Int64 "Int64"

    justBase @Word "Word"
    justBase @Word8 "Word8"
    justBase @Word16 "Word16"
    justBase @Word32 "Word32"
    justBase @Word64 "Word64"

    justBase @Natural "Natural"
    justBase @Integer "Integer"

    justBase @Float "Float"
    justBase @Double "Double"

    -- justBase @Centi "Centi"
    -- justBase @Rational "Rational"

justBase :: forall a. (Prelude.Eq a, Show a, GenValid a, CommutativeRing a)
    => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. (Prelude.Eq a, CommutativeRing a, Show a, GenValid a)
    => String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "(*)" do
            prop "commutative" (commutative @a (*))
