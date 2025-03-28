{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Eq.PartialEq
    ( PartialEq (..)
    , incomparable
    , partialEqVia
    , partialEqOn
    ) where

import GHC.Generics
import GHC.Records (HasField (getField))
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified

class PartialEq a where
    (~=) :: a -> a -> Bool
    default (~=) :: (Generic a, GPartialEq (Rep a)) => a -> a -> Bool
    (~=) x y = geqPartial (from x) (from y)
    {-# INLINE (~=) #-}

    (==?) :: a -> a -> Maybe Bool
    default (==?) :: (Generic a, GPartialEq (Rep a)) => a -> a -> Maybe Bool
    (==?) x y = geqMaybe (from x) (from y)
    {-# INLINE (==?) #-}

    (/~=) :: a -> a -> Bool
    (/~=) x y = not (x ~= y)
    {-# INLINE (/~=) #-}

infix 4 ~=, ==?, /~=

incomparable :: (PartialEq a) => a -> a -> Bool
incomparable x y = (x ==? y) ~= Nothing

partialEqVia :: (PartialEq b) => (a -> b) -> a -> a -> Bool
partialEqVia f x y = f x ~= f y

partialEqOn :: forall f r a. (HasField f r a, PartialEq a) => r -> r -> Bool
partialEqOn = partialEqVia (getField @f)

--------------------------------------------------------------------------

class GPartialEq f where
    geqPartial :: (f a) -> (f a) -> Bool
    geqMaybe :: (f a) -> (f a) -> Maybe Bool

instance GPartialEq V1 where
    geqPartial = undefined
    geqMaybe = undefined

instance GPartialEq U1 where
    geqPartial _ _ = True
    geqMaybe _ _ = Just True

instance (GPartialEq f, GPartialEq g) => GPartialEq (f :+: g) where
    geqPartial (L1 x) (L1 y) = geqPartial x y
    geqPartial (R1 x) (R1 y) = geqPartial x y
    geqPartial _ _ = False

    geqMaybe (L1 x) (L1 y) = geqMaybe x y
    geqMaybe (R1 x) (R1 y) = geqMaybe x y
    geqMaybe _ _ = Just False

instance (GPartialEq f, GPartialEq g) => GPartialEq (f :*: g) where
    geqPartial (x1 :*: y1) (x2 :*: y2) = geqPartial x1 x2 && geqPartial y1 y2
    geqMaybe (x1 :*: y1) (x2 :*: y2) = liftA2 (&&) (geqMaybe x1 x2) (geqMaybe y1 y2)

instance (PartialEq c) => GPartialEq (K1 i c) where
    geqPartial (K1 x) (K1 y) = x ~= y
    geqMaybe (K1 x) (K1 y) = x ==? y

instance (GPartialEq f) => GPartialEq (M1 i t f) where
    geqPartial (M1 x) (M1 y) = geqPartial x y
    geqMaybe (M1 x) (M1 y) = geqMaybe x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => PartialEq (FromPrelude a) where
    (~=) = liftPrelude2 (Prelude.==)
    x ==? y = Just $ liftPrelude2 (Prelude.==) x y

deriving via (FromPrelude Ordering) instance PartialEq Ordering
deriving via (FromPrelude Bool) instance PartialEq Bool
deriving via (FromPrelude Int) instance PartialEq Int

--------------------------------------------------------------------------

deriving anyclass instance (PartialEq a) => PartialEq (Maybe a)
deriving anyclass instance (PartialEq a) => PartialEq [a]
deriving anyclass instance (PartialEq a, PartialEq b) => PartialEq (Either a b)

deriving anyclass instance (PartialEq a, PartialEq b) => PartialEq (a, b)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c) => PartialEq (a, b, c)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c, PartialEq d) => PartialEq (a, b, c, d)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e) => PartialEq (a, b, c, d, e)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f)
    => PartialEq (a, b, c, d, e, f)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g)
    => PartialEq (a, b, c, d, e, f, g)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h)
    => PartialEq (a, b, c, d, e, f, g, h)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i)
    => PartialEq (a, b, c, d, e, f, g, h, i)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j)
    => PartialEq (a, b, c, d, e, f, g, h, i, j)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j, PartialEq k)
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j, PartialEq k, PartialEq l)
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j, PartialEq k, PartialEq l, PartialEq m)
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j, PartialEq k, PartialEq l, PartialEq m, PartialEq n)
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h, PartialEq i, PartialEq j, PartialEq k, PartialEq l, PartialEq m, PartialEq n, PartialEq o)
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
