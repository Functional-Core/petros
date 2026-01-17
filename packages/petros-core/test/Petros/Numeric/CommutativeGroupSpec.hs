{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Petros.Numeric.CommutativeGroupSpec (spec) where

import Data.GenValidity
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Arbitrary)
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
import Petros.Algebra.Group (Group (..))
import Petros.Algebra.Group (CommutativeGroup)

spec :: Spec
spec = describe "Semigroup" do
    describe "Sum" do
        justBase @(Sum Int) "Int"
        justBase @(Sum Integer) "Integer"


        -- justBase @(Sum Centi) "Centi"
        -- justBase @(Sum Rational) "Rational"
        
    describe "Product" do
        pure ()

        -- justBase @(Product Centi) "Centi"
        -- justBase @(Product Rational) "Rational"

justBase :: forall a. ( Prelude.Eq a, CommutativeGroup a, Show a, GenValid a)
    =>String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. ( Prelude.Eq a, CommutativeGroup a, Show a, GenValid a)
    =>String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "(<>)" do
            prop "commutative" (commutative @a (<>))
