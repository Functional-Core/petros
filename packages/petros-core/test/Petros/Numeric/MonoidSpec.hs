{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Numeric.MonoidSpec (spec) where

import Data.GenValidity
import Petros.Eq.Eq
import Petros.Eq.PartialEq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.Validity.Property
import Prelude hiding (Eq (..))
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

spec :: Spec
spec = describe "Monoid" do
    describe "Sum" do
        justBase @(Sum Int) "Int"
        justBase @(Sum Int8) "Int8"
        justBase @(Sum Int16) "Int16"
        justBase @(Sum Int32) "Int32"
        justBase @(Sum Int64) "Int64"

        justBase @(Sum Word) "Word"
        justBase @(Sum Word8) "Word8"
        justBase @(Sum Word16) "Word16"
        justBase @(Sum Word32) "Word32"
        justBase @(Sum Word64) "Word64"

        justBase @(Sum Natural) "Natural"
        justBase @(Sum Integer) "Integer"

        justBase @(Sum Float) "Float"
        justBase @(Sum Double) "Double"

        -- justBase @(Sum Centi) "Centi"
        -- justBase @(Sum Rational) "Rational"
        
    describe "Product" do
        justBase @(Product Int) "Int"
        justBase @(Product Int8) "Int8"
        justBase @(Product Int16) "Int16"
        justBase @(Product Int32) "Int32"
        justBase @(Product Int64) "Int64"

        justBase @(Product Word) "Word"
        justBase @(Product Word8) "Word8"
        justBase @(Product Word16) "Word16"
        justBase @(Product Word32) "Word32"
        justBase @(Product Word64) "Word64"

        justBase @(Product Natural) "Natural"
        justBase @(Product Integer) "Integer"

        justBase @(Product Float) "Float"
        justBase @(Product Double) "Double"

        -- justBase @(Product Centi) "Centi"
        -- justBase @(Product Rational) "Rational"

justBase :: forall a. (Prelude.Eq a, Monoid a, Show a, GenValid a)
    => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. (Prelude.Eq a, Monoid a, Show a, GenValid a)
    => String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "mempty" do
            prop "left identity for (<>)" (leftIdentity @a (<>) mempty)
            prop "right identity for (<>)" (rightIdentity @a (<>) mempty)
