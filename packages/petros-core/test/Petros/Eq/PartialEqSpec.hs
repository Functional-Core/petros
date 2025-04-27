{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Eq.PartialEqSpec (spec) where

import Petros.Eq.PartialEq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.Validity.Property
import Prelude hiding (Eq (..))
import Prelude qualified

spec :: Spec
spec = describe "PartialEq" do
    justBase @Bool "Bool"
    justBase @Int "Int"
    justBase @Char "Char"
    baseSpec @Float "Float (IEEE 754)" (ieeeExtras @Float)
    baseSpec @Double "Double (IEEE 754)" (ieeeExtras @Double)
    justBase @(Maybe Int) "Maybe Int"
    justBase @(Int, Char) "(Int, Char)"
    justBase @([Int]) "[Int]"
    justBase @(Either Int Char) "Either Int Char"
    justBase @(Ordering) "Ordering"

ieeeExtras :: forall a. (RealFrac a, PartialEq a) => Spec
ieeeExtras = do
    it "0 ~= -0" $ (0 :: a) ~= -0
    it "NaN /~= NaN" $ not (nan @a ~= nan)
    it "NaN ==? NaN = Nothing" $ (nan @a ==? nan) Prelude.== Nothing
    it "Infinity ~= Infinity" $ infinity @a ~= infinity
    it "-Infinity ~= -Infinity" $ negInfinity @a ~= negInfinity
    it "Infinity /~= -Infinity" $ infinity @a /~= negInfinity

nan :: (RealFrac a) => a
nan = 0 / 0

infinity :: (RealFrac a) => a
infinity = 1 / 0

negInfinity :: (RealFrac a) => a
negInfinity = -1 / 0

negation
    :: forall a
     . (Show a, GenValid a)
    => (a -> a -> Bool)
    -> (a -> a -> Bool)
    -> Property
negation op negOp = forAllValid @a \x ->
    forAllValid \y -> not (x `op` y) Prelude.== (x `negOp` y)

consistency :: forall a. (PartialEq a, Show a, GenValid a) => Property
consistency = forAllValid @a \x ->
    forAllValid \y -> case x ==? y of
        Nothing -> True
        Just b -> b Prelude.== (x ~= y)

symmetryMaybe :: forall a. (PartialEq a, Show a, GenValid a) => Property
symmetryMaybe = forAllValid @a \x ->
    forAllValid \y -> (x ==? y) Prelude.== (y ==? x)

irreflexivity :: forall a. (PartialEq a, Show a, GenValid a) => Property
irreflexivity = forAllValid @a \x ->
    if not (x ~= x)
        then (x ==? x) Prelude.== Nothing
        else True

irreflexiveToIncomparable :: forall a. (PartialEq a, Show a, GenValid a) => Property
irreflexiveToIncomparable = forAllValid @a \x ->
    forAllValid \y ->
        if not (x ~= x)
            then incomparable x y
            else True

incomparableToFalse :: forall a. (PartialEq a, Show a, GenValid a) => Property
incomparableToFalse = forAllValid @a \x ->
    forAllValid \y ->
        if (x ==? y) Prelude.== Nothing
            then (x ~= y) Prelude.== False
            else True

justBase :: forall a. (PartialEq a, Show a, GenValid a) => String -> Spec
justBase label = baseSpec @a label (pure ())

baseSpec
    :: forall a
     . (PartialEq a, Show a, GenValid a)
    => String
    -> Spec
    -> Spec
baseSpec label extras = do
    describe label do
        extras
        describe "(~=)" do
            prop "symmetry" (symmetry @a (~=))
            prop "transitivity" (transitivity @a (~=))
            prop "negation of (/~=)" (negation @a (~=) (/~=))
            prop "False if (==?) returns Nothing" (incomparableToFalse @a)
        describe "(==?)" do
            prop "is the same as (~=) when is (Just b)" (consistency @a)
            prop "symmetry" (symmetryMaybe @a)
            prop "irreflexivity" (irreflexivity @a)
            prop "irreflexive means incomparable" (irreflexiveToIncomparable @a)
        describe "(~/=)" do
            prop "symmetry" (symmetry @a (/~=))
            prop "negation of (~=)" (negation @a (/~=) (~=))
