{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}

module Petros.Eq.PartialEq
    ( PartialHEq (..)
    , PartialEq
    , (~=)
    , (==?)
    , (/~=)
    ) where

import GHC.Generics
import Prelude hiding (Eq (..))
import Prelude qualified
import Petros.Internal

class PartialHEq a b where
    (~==) :: a -> b -> Bool
    default (~==) :: (Generic a, Generic b, GPartialHeq (Rep a) (Rep b)) => a -> b -> Bool
    (~==) x y = gheqPartial (from x) (from y)
    {-# INLINE (~==) #-}

    (===?) :: a -> b -> Maybe Bool
    default (===?) :: (Generic a, Generic b, GPartialHeq (Rep a) (Rep b)) => a -> b -> Maybe Bool
    (===?) x y = gheqMaybe (from x) (from y)
    {-# INLINE (===?) #-}

    (/~==) :: a -> b -> Bool
    (/~==) x y = not (x ~== y)
    {-# INLINE (/~==) #-}

type PartialEq a = PartialHEq a a

(~=) :: PartialEq a => a -> a -> Bool
(~=) = (~==)
{-# INLINE (~=) #-}

(==?) :: PartialEq a => a -> a -> Maybe Bool
(==?) = (===?)
{-# INLINE (==?) #-}

(/~=) :: PartialEq a => a -> a -> Bool
(/~=) = (/~==)
{-# INLINE (/~=) #-}

infix 4 ~==, ===?, /~==, ~=, ==?, /~=

--------------------------------------------------------------------------

class GPartialHeq f g where
    gheqPartial :: (f a) -> (g a) -> Bool
    gheqMaybe :: (f a) -> (g a) -> Maybe Bool

instance {-# OVERLAPPING #-} GPartialHeq V1 V1 where
    gheqPartial = undefined
    gheqMaybe = undefined

instance GPartialHeq V1 g where
    gheqPartial = undefined
    gheqMaybe = undefined

instance GPartialHeq f V1 where
    gheqPartial = undefined
    gheqMaybe = undefined

instance {-# OVERLAPPING #-} GPartialHeq U1 U1 where
    gheqPartial _ _ = True
    gheqMaybe _ _ = Just True

instance GPartialHeq U1 g where
    gheqPartial _ _ = False
    gheqMaybe _ _ = Just False

instance GPartialHeq f U1 where
    gheqPartial _ _ = False
    gheqMaybe _ _ = Just False

instance (GPartialHeq f1 g1, GPartialHeq f1 g2, GPartialHeq f2 g1, GPartialHeq f2 g2) => GPartialHeq (f1 :+: f2) (g1 :+: g2) where
    gheqPartial (L1 x) (L1 y) = gheqPartial x y
    gheqPartial (L1 x) (R1 y) = gheqPartial x y
    gheqPartial (R1 x) (L1 y) = gheqPartial x y
    gheqPartial (R1 x) (R1 y) = gheqPartial x y

    gheqMaybe (L1 x) (L1 y) = gheqMaybe x y
    gheqMaybe (L1 x) (R1 y) = gheqMaybe x y
    gheqMaybe (R1 x) (L1 y) = gheqMaybe x y
    gheqMaybe (R1 x) (R1 y) = gheqMaybe x y

instance (GPartialHeq f g, GPartialHeq f2 g2) => GPartialHeq (f :*: f2) (g :*: g2) where
    gheqPartial (x1 :*: x2) (y1 :*: y2) = gheqPartial x1 y1 && gheqPartial x2 y2
    gheqMaybe (x1 :*: x2) (y1 :*: y2) = liftA2 (&&) (gheqMaybe x1 y1) (gheqMaybe x2 y2)

instance (PartialHEq c d) => GPartialHeq (K1 i c) (K1 j d) where
    gheqPartial (K1 x) (K1 y) = x ~== y
    gheqMaybe (K1 x) (K1 y) = x ===? y

instance (GPartialHeq f g) => GPartialHeq (M1 i t f) (M1 j u g) where
    gheqPartial (M1 x) (M1 y) = gheqPartial x y
    gheqMaybe (M1 x) (M1 y) = gheqMaybe x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => PartialHEq (FromPrelude a) (FromPrelude a) where
    (~==) = liftPrelude2 (Prelude.==)
    x ===? y = Just $ liftPrelude2 (Prelude.==) x y

instance (Prelude.Eq a) => PartialHEq a (FromPrelude a) where
    x ~== y = liftPrelude (x Prelude.==) y
    x ===? y = Just $ liftPrelude (x Prelude.==) y

instance (Prelude.Eq a) => PartialHEq (FromPrelude a) a where
    x ~== y = liftPrelude (Prelude.== y) x
    x ===? y = Just $ liftPrelude (Prelude.== y) x

deriving via (FromPrelude Ordering) instance PartialHEq Ordering Ordering

--------------------------------------------------------------------------

deriving anyclass instance (PartialHEq a b) => PartialHEq (Maybe a) (Maybe b)
deriving anyclass instance (PartialHEq a c, PartialHEq a d, PartialHEq b c, PartialHEq b d) => PartialHEq (Either a b) (Either c d)

deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2)
    => PartialHEq (a1, b1) (a2, b2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2)
    => PartialHEq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2)
    => PartialHEq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2)
    => PartialHEq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2)
    => PartialHEq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2, PartialHEq k1 k2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2, PartialHEq k1 k2, PartialHEq l1 l2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2, PartialHEq k1 k2, PartialHEq l1 l2, PartialHEq m1 m2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2, PartialHEq k1 k2, PartialHEq l1 l2, PartialHEq m1 m2, PartialHEq n1 n2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (PartialHEq a1 a2, PartialHEq b1 b2, PartialHEq c1 c2, PartialHEq d1 d2, PartialHEq e1 e2, PartialHEq f1 f2, PartialHEq g1 g2, PartialHEq h1 h2, PartialHEq i1 i2, PartialHEq j1 j2, PartialHEq k1 k2, PartialHEq l1 l2, PartialHEq m1 m2, PartialHEq n1 n2, PartialHEq o1 o2)
    => PartialHEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
