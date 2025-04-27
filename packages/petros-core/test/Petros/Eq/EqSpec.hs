{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Petros.Eq.EqSpec (spec) where

import Data.GenValidity
import Petros.Eq.Eq
import Petros.Eq.PartialEq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.Validity.Property
import Prelude hiding (Eq (..))
import Prelude qualified

spec :: Spec
spec = describe "Eq" do
    justBase @Bool "Bool"
    justBase @Char "Char"
    justBase @Int "Int"
    justBase @(Int, Char) "(Int, Char)"
    justBase @(Maybe Int) "Maybe Int"
    justBase @[Int] "[Int]"
    justBase @(Either Int Char) "Either Int Char"

negation
    :: forall a
     . (Show a, GenValid a)
    => (a -> a -> Bool)
    -> (a -> a -> Bool)
    -> Property
negation op negOp = forAllValid @a \x ->
    forAllValid \y -> not (x `op` y) Prelude.== (x `negOp` y)

justBase :: forall a. (Eq a, Show a, GenValid a) => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec :: forall a. (Eq a, Show a, GenValid a) => String -> Spec -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "(==)" do
            prop "symmetry" (symmetry @a (==))
            prop "negation of (/=)" (negation @a (==) (/=))
            prop "reflexive" (reflexivity @a (==))
            prop "consistent with (~=)" (equivalent2 @a (==) (~=))
        describe "(/=)" do
            prop "symmetry" (symmetry @a (/=))
            prop "negation of (==)" (negation @a (/=) (==))
            prop "consistent with (/~=)" (equivalent2 @a (/=) (/~=))
