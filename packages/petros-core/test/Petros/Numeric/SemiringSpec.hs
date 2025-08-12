{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Numeric.SemiringSpec (spec) where

import Data.GenValidity
import Petros.Eq.Eq
import Petros.Eq.PartialEq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Arbitrary)
import Test.Validity.Property
import Prelude hiding (Num (..), Eq (..))
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
import Petros.Algebra.Semiring (Semiring (..))

spec :: Spec
spec = describe "Semiring" do
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
        
justBase :: forall a. (Prelude.Eq a, Semiring a, Show a, GenValid a, Arbitrary a)
    => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. (Prelude.Eq a, Semiring a, Show a, GenValid a, Arbitrary a)
    => String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "(+)" do
            prop "zero left identity" (leftIdentity @a (+) zero)
            prop "zero right identity" (rightIdentity @a (+) zero)
            prop "associative" (associative @a (+))
            prop "commutative" (commutative @a (+))
        describe "(*)" do
            prop "one left identity" (leftIdentity @a (*) one)
            prop "one right identity" (rightIdentity @a (*) one)
            prop "zero left annihilation" (\(x :: a) -> zero * x Prelude.== zero)
            prop "zero right annihilation" (\(x :: a) -> x * zero Prelude.== zero)
            prop "left distributive over (+)"
                (\(x :: a) y z -> x * (y + z) Prelude.== (x * y) + (x * z))
            prop "right distributive over (+)"
                (\(x :: a) y z -> (y + z) * x Prelude.== (y * x) + (z * x))
            -- TODO: helper functions for annihilation and distributivity
