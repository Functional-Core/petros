{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}

module Petros.Eq.PartialEq
    ( HetPartialEq (..)
    , PartialEq
    , incomparable
    , (~==)
    , (===?)
    , (/~==)
    , law_partial
    , law_negation
    , law_symmetry
    , law_transitivity
    , law_extensionality
    ) where

import GHC.Generics
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified

class HetPartialEq a b where
    (~=) :: a -> b -> Bool
    default (~=) :: (Generic a, Generic b, GPartialEq (Rep a) (Rep b)) => a -> b -> Bool
    (~=) x y = geqPartial (from x) (from y)
    {-# INLINE (~=) #-}

    (==?) :: a -> b -> Maybe Bool
    default (==?) :: (Generic a, Generic b, GPartialEq (Rep a) (Rep b)) => a -> b -> Maybe Bool
    (==?) x y = geqMaybe (from x) (from y)
    {-# INLINE (==?) #-}

    (/~=) :: a -> b -> Bool
    (/~=) x y = not (x ~= y)
    {-# INLINE (/~=) #-}

incomparable :: (HetPartialEq a b) => a -> b -> Bool
incomparable x y = (x ==? y) ~== Nothing

law_partial :: (HetPartialEq a b) => a -> b -> Bool
law_partial x y = ((x ==? y) Prelude.== Just True) Prelude.== (x ~= y)

law_negation :: (HetPartialEq a b) => a -> b -> Bool
law_negation x y =
    ((x /~= y) Prelude.== not (x ~= y))
        || incomparable x y

law_symmetry :: (HetPartialEq a b, HetPartialEq b a) => a -> b -> Bool
law_symmetry x y = (x ~= y) Prelude.== (y ~= x)

law_transitivity :: (HetPartialEq a b, HetPartialEq b c, HetPartialEq a c) => a -> b -> c -> Bool
law_transitivity x y z = not (x ~= y && y ~= z) || (x ~= z)

law_extensionality :: (PartialEq a, PartialEq b) => (a -> b) -> a -> a -> Bool
law_extensionality f x y = (x ~== y) Prelude.== (f x ~== f y)

type PartialEq a = HetPartialEq a a

(~==) :: (PartialEq a) => a -> a -> Bool
(~==) = (~=)
{-# INLINE (~==) #-}

(===?) :: (PartialEq a) => a -> a -> Maybe Bool
(===?) = (==?)
{-# INLINE (===?) #-}

(/~==) :: (PartialEq a) => a -> a -> Bool
(/~==) = (/~=)
{-# INLINE (/~==) #-}

infix 4 ~=, ==?, /~=, ~==, ===?, /~==

--------------------------------------------------------------------------

class GPartialEq f g where
    geqPartial :: (f a) -> (g a) -> Bool
    geqMaybe :: (f a) -> (g a) -> Maybe Bool

instance {-# OVERLAPPING #-} GPartialEq V1 V1 where
    geqPartial = undefined
    geqMaybe = undefined

instance GPartialEq V1 g where
    geqPartial = undefined
    geqMaybe = undefined

instance GPartialEq f V1 where
    geqPartial = undefined
    geqMaybe = undefined

instance {-# OVERLAPPING #-} GPartialEq U1 U1 where
    geqPartial _ _ = True
    geqMaybe _ _ = Just True

instance GPartialEq U1 g where
    geqPartial _ _ = False
    geqMaybe _ _ = Just False

instance GPartialEq f U1 where
    geqPartial _ _ = False
    geqMaybe _ _ = Just False

instance (GPartialEq f1 g1, GPartialEq f2 g2) => GPartialEq (f1 :+: f2) (g1 :+: g2) where
    geqPartial (L1 x) (L1 y) = geqPartial x y
    geqPartial (R1 x) (R1 y) = geqPartial x y
    geqPartial _ _ = False

    geqMaybe (L1 x) (L1 y) = geqMaybe x y
    geqMaybe (R1 x) (R1 y) = geqMaybe x y
    geqMaybe _ _ = Just False

instance (GPartialEq f g, GPartialEq f2 g2) => GPartialEq (f :*: f2) (g :*: g2) where
    geqPartial (x1 :*: x2) (y1 :*: y2) = geqPartial x1 y1 && geqPartial x2 y2
    geqMaybe (x1 :*: x2) (y1 :*: y2) = liftA2 (&&) (geqMaybe x1 y1) (geqMaybe x2 y2)

instance (HetPartialEq c d) => GPartialEq (K1 i c) (K1 j d) where
    geqPartial (K1 x) (K1 y) = x ~= y
    geqMaybe (K1 x) (K1 y) = x ==? y

instance (GPartialEq f g) => GPartialEq (M1 i t f) (M1 j u g) where
    geqPartial (M1 x) (M1 y) = geqPartial x y
    geqMaybe (M1 x) (M1 y) = geqMaybe x y

--------------------------------------------------------------------------

instance (HetPartialEq b a) => HetPartialEq a (Flipped b) where
    (~=) = liftFlipped (~=)
    (==?) = liftFlipped (==?)

--------------------------------------------------------------------------

instance (Prelude.Eq a) => HetPartialEq (FromPrelude a) (FromPrelude a) where
    (~=) = liftPrelude2 (Prelude.==)
    x ==? y = Just $ liftPrelude2 (Prelude.==) x y

instance (Prelude.Eq a) => HetPartialEq a (FromPrelude a) where
    x ~= y = liftPrelude (x Prelude.==) y
    x ==? y = Just $ liftPrelude (x Prelude.==) y

instance (Prelude.Eq a) => HetPartialEq (FromPrelude a) a where
    x ~= y = liftPrelude (Prelude.== y) x
    x ==? y = Just $ liftPrelude (Prelude.== y) x

deriving via (FromPrelude Ordering) instance HetPartialEq Ordering Ordering
deriving via (FromPrelude Bool) instance HetPartialEq Bool Bool
deriving via (FromPrelude Int) instance HetPartialEq Int Int

--------------------------------------------------------------------------

deriving anyclass instance (HetPartialEq a b) => HetPartialEq (Maybe a) (Maybe b)
deriving anyclass instance (HetPartialEq a b) => HetPartialEq [a] [b]
deriving anyclass instance (HetPartialEq a c, HetPartialEq b d) => HetPartialEq (Either a b) (Either c d)

deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2)
    => HetPartialEq (a1, b1) (a2, b2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2)
    => HetPartialEq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2)
    => HetPartialEq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2)
    => HetPartialEq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2, HetPartialEq k1 k2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2, HetPartialEq k1 k2, HetPartialEq l1 l2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2, HetPartialEq k1 k2, HetPartialEq l1 l2, HetPartialEq m1 m2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2, HetPartialEq k1 k2, HetPartialEq l1 l2, HetPartialEq m1 m2, HetPartialEq n1 n2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (HetPartialEq a1 a2, HetPartialEq b1 b2, HetPartialEq c1 c2, HetPartialEq d1 d2, HetPartialEq e1 e2, HetPartialEq f1 f2, HetPartialEq g1 g2, HetPartialEq h1 h2, HetPartialEq i1 i2, HetPartialEq j1 j2, HetPartialEq k1 k2, HetPartialEq l1 l2, HetPartialEq m1 m2, HetPartialEq n1 n2, HetPartialEq o1 o2)
    => HetPartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
