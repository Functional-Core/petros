{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Numeric.GroupSpec (spec) where

import Data.GenValidity
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Arbitrary)
import Test.Validity.Property
import Prelude hiding (Eq (..))
import Prelude qualified
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
import Petros.Algebra.Group (Sum (..), Product (..), Group (..))

spec :: Spec
spec = describe "Group" do
    describe "Sum" do
        justBase @(Sum Int) "Int"
        justBase @(Sum Int8) "Int8"
        justBase @(Sum Int16) "Int16"
        justBase @(Sum Int32) "Int32"
        justBase @(Sum Int64) "Int64"
        justBase @(Sum Integer) "Integer"


        -- justBase @(Sum Centi) "Centi"
        -- justBase @(Sum Rational) "Rational"
        
    describe "Product" do
        pure ()

        -- justBase @(Product Centi) "Centi"
        -- justBase @(Product Rational) "Rational"

justBase :: forall a. (Arbitrary a, Prelude.Eq a, Group a, Show a)
    => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. (Arbitrary a, Prelude.Eq a, Group a, Show a)
    => String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "inverse" do
            prop "left inverse for (<>)"
                (\(x :: a) -> inverse x <> x Prelude.== mempty)
            prop "right inverse for (<>)"
                (\(x :: a) -> x <> inverse x Prelude.== mempty)
