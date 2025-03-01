{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

module Petros.Ord
    ( Ordering (..)
    , PartialOrd (..)
    , (<=>?)
    , Ord (..)
    , (<=>)
    ) where

import Control.Applicative ((<|>))
import GHC.Generics
import Petros.Eq
import Prelude hiding (Eq (..), Ord (..))

class (PartialEq a) => PartialOrd a where
    cmpPartial :: a -> a -> Maybe Ordering
    default cmpPartial :: (Generic a, GPartialOrd (Rep a)) => a -> a -> Maybe Ordering
    cmpPartial x y = gcmpPartial (from x) (from y)

    (<.) :: a -> a -> Bool
    x <. y = case cmpPartial x y of
        Just LT -> True
        _ -> False

    (<=.) :: a -> a -> Bool
    x <=. y = case cmpPartial x y of
        Just LT -> True
        Just EQ -> True
        _ -> False

    (>.) :: a -> a -> Bool
    x >. y = case cmpPartial x y of
        Just GT -> True
        _ -> False

    (>=.) :: a -> a -> Bool
    x >=. y = case cmpPartial x y of
        Just GT -> True
        Just EQ -> True
        _ -> False

    (>/<) :: a -> a -> Bool
    x >/< y = case cmpPartial x y of
        Nothing -> True
        _ -> False

(<=>?) :: (PartialOrd a) => a -> a -> Maybe Ordering
(<=>?) = cmpPartial

class (Eq a, PartialOrd a) => Ord a where
    cmp :: a -> a -> Ordering
    cmp x y =
        if x == y
            then EQ
            else
                if x <= y
                    then LT
                    else GT

    (<=) :: a -> a -> Bool
    x <= y = case cmp x y of
        GT -> False
        _ -> True

    (>=) :: a -> a -> Bool
    x >= y = y <= x

    (>) :: a -> a -> Bool
    x > y = not (x <= y)

    (<) :: a -> a -> Bool
    x < y = not (x >= y)

    min :: a -> a -> a
    min x y = if x <= y then x else y

    max :: a -> a -> a
    max x y = if x <= y then y else x

    {-# MINIMAL cmp | (<=) #-}

(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = cmp

--------------------------------------------------
-- Generics
--------------------------------------------------

-- PartialOrd

class GPartialOrd f where
    gcmpPartial :: (f a) -> (f a) -> Maybe Ordering

instance GPartialOrd V1 where
    gcmpPartial = undefined

instance GPartialOrd U1 where
    gcmpPartial _ _ = Just EQ

instance (GPartialOrd f, GPartialOrd g) => GPartialOrd (f :+: g) where
    gcmpPartial (L1 x) (L1 y) = gcmpPartial x y
    gcmpPartial (R1 x) (R1 y) = gcmpPartial x y
    gcmpPartial _ _ = Nothing

instance (GPartialOrd f, GPartialOrd g) => GPartialOrd (f :*: g) where
    gcmpPartial (x1 :*: y1) (x2 :*: y2) =
        if ordx == ordy || ordy == Just EQ
            then ordx
            else Nothing
        where
            ordx = gcmpPartial x1 x2
            ordy = gcmpPartial y1 y2

instance (PartialOrd c) => GPartialOrd (K1 i c) where
    gcmpPartial (K1 x) (K1 y) = cmpPartial x y

instance (GPartialOrd c) => GPartialOrd (M1 i j c) where
    gcmpPartial (M1 x) (M1 y) = gcmpPartial x y

-- Ord

class GOrd f where
    gcmp :: (f a) -> (f a) -> Ordering

instance GOrd V1 where
    gcmp = undefined

instance GOrd U1 where
    gcmp _ _ = EQ

-- There is no instance for (:+:) or (:*:) due
-- to there always being incomparable cases.

instance (Ord c) => GOrd (K1 i c) where
    gcmp (K1 x) (K1 y) = cmp x y

instance (GOrd c) => GOrd (M1 i j c) where
    gcmp (M1 x) (M1 y) = gcmp x y

 
--------------------------------------------------
-- Strict ordering
--------------------------------------------------

class GStrictPartialOrd f where
    gcmpStrictPartial :: (f a) -> (f a) -> Maybe Ordering

instance GStrictPartialOrd V1 where
    gcmpStrictPartial = undefined

instance GStrictPartialOrd U1 where
    gcmpStrictPartial _ _ = Just EQ

instance (GStrictPartialOrd f, GStrictPartialOrd g) => GStrictPartialOrd (f :+: g) where
    gcmpStrictPartial (L1 x) (L1 y) = gcmpStrictPartial x y
    gcmpStrictPartial (R1 x) (R1 y) = gcmpStrictPartial x y
    gcmpStrictPartial _ _ = Nothing

instance (GStrictPartialOrd f, GStrictPartialOrd g) => GStrictPartialOrd (f :*: g) where
    gcmpStrictPartial (x1 :*: y1) (x2 :*: y2) =
        if ordx == ordy
            then ordx
            else Nothing
        where
            ordx = gcmpStrictPartial x1 x2
            ordy = gcmpStrictPartial y1 y2

instance (PartialOrd c) => GStrictPartialOrd (K1 i c) where
    gcmpStrictPartial (K1 x) (K1 y) = cmpPartial x y

instance (GStrictPartialOrd c) => GStrictPartialOrd (M1 i j c) where
    gcmpStrictPartial (M1 x) (M1 y) = gcmpStrictPartial x y

newtype Strict a = Strict a
    deriving stock (Generic)
    deriving newtype (Show, PartialEq, Eq)

instance (Generic a, PartialEq a, GStrictPartialOrd (Rep a)) => PartialOrd (Strict a) where
    cmpPartial (Strict x) (Strict y) = gcmpStrictPartial (from x) (from y)

--------------------------------------------------
-- Instances
--------------------------------------------------
