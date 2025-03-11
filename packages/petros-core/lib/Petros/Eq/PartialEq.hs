{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}

module Petros.Eq.PartialEq
    ( PartialEq (..)
    , PartialEq_
    , (~=)
    , (==?)
    , (/~=)
    ) where

import GHC.Generics
import Prelude hiding (Eq (..))
import Prelude qualified
import Petros.Internal

class PartialEq a b where
    (~==) :: a -> b -> Bool
    default (~==) :: (Generic a, Generic b, GPartialEq (Rep a) (Rep b)) => a -> b -> Bool
    (~==) x y = geqPartial (from x) (from y)
    {-# INLINE (~==) #-}

    (===?) :: a -> b -> Maybe Bool
    default (===?) :: (Generic a, Generic b, GPartialEq (Rep a) (Rep b)) => a -> b -> Maybe Bool
    (===?) x y = geqMaybe (from x) (from y)
    {-# INLINE (===?) #-}

    (/~==) :: a -> b -> Bool
    (/~==) x y = not (x ~== y)
    {-# INLINE (/~==) #-}

type PartialEq_ a = PartialEq a a

(~=) :: PartialEq_ a => a -> a -> Bool
(~=) = (~==)
{-# INLINE (~=) #-}

(==?) :: PartialEq_ a => a -> a -> Maybe Bool
(==?) = (===?)
{-# INLINE (==?) #-}

(/~=) :: PartialEq_ a => a -> a -> Bool
(/~=) = (/~==)
{-# INLINE (/~=) #-}

infix 4 ~==, ===?, /~==, ~=, ==?, /~=

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

instance (GPartialEq f1 g1, GPartialEq f1 g2, GPartialEq f2 g1, GPartialEq f2 g2) => GPartialEq (f1 :+: f2) (g1 :+: g2) where
    geqPartial (L1 x) (L1 y) = geqPartial x y
    geqPartial (L1 x) (R1 y) = geqPartial x y
    geqPartial (R1 x) (L1 y) = geqPartial x y
    geqPartial (R1 x) (R1 y) = geqPartial x y

    geqMaybe (L1 x) (L1 y) = geqMaybe x y
    geqMaybe (L1 x) (R1 y) = geqMaybe x y
    geqMaybe (R1 x) (L1 y) = geqMaybe x y
    geqMaybe (R1 x) (R1 y) = geqMaybe x y

instance (GPartialEq f g, GPartialEq f2 g2) => GPartialEq (f :*: f2) (g :*: g2) where
    geqPartial (x1 :*: x2) (y1 :*: y2) = geqPartial x1 y1 && geqPartial x2 y2
    geqMaybe (x1 :*: x2) (y1 :*: y2) = liftA2 (&&) (geqMaybe x1 y1) (geqMaybe x2 y2)

instance (PartialEq c d) => GPartialEq (K1 i c) (K1 j d) where
    geqPartial (K1 x) (K1 y) = x ~== y
    geqMaybe (K1 x) (K1 y) = x ===? y

instance (GPartialEq f g) => GPartialEq (M1 i t f) (M1 j u g) where
    geqPartial (M1 x) (M1 y) = geqPartial x y
    geqMaybe (M1 x) (M1 y) = geqMaybe x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => PartialEq (FromPrelude a) (FromPrelude a) where
    (~==) = liftPrelude2 (Prelude.==)
    x ===? y = Just $ liftPrelude2 (Prelude.==) x y

instance (Prelude.Eq a) => PartialEq a (FromPrelude a) where
    x ~== y = liftPrelude (x Prelude.==) y
    x ===? y = Just $ liftPrelude (x Prelude.==) y

instance (Prelude.Eq a) => PartialEq (FromPrelude a) a where
    x ~== y = liftPrelude (Prelude.== y) x
    x ===? y = Just $ liftPrelude (Prelude.== y) x

deriving via (FromPrelude Ordering) instance PartialEq Ordering Ordering

--------------------------------------------------------------------------

deriving anyclass instance (PartialEq a b) => PartialEq (Maybe a) (Maybe b)
deriving anyclass instance (PartialEq a c, PartialEq a d, PartialEq b c, PartialEq b d) => PartialEq (Either a b) (Either c d)

deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2)
    => PartialEq (a1, b1) (a2, b2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2)
    => PartialEq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2)
    => PartialEq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2)
    => PartialEq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2)
    => PartialEq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2, PartialEq k1 k2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2, PartialEq k1 k2, PartialEq l1 l2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2, PartialEq k1 k2, PartialEq l1 l2, PartialEq m1 m2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2, PartialEq k1 k2, PartialEq l1 l2, PartialEq m1 m2, PartialEq n1 n2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (PartialEq a1 a2, PartialEq b1 b2, PartialEq c1 c2, PartialEq d1 d2, PartialEq e1 e2, PartialEq f1 f2, PartialEq g1 g2, PartialEq h1 h2, PartialEq i1 i2, PartialEq j1 j2, PartialEq k1 k2, PartialEq l1 l2, PartialEq m1 m2, PartialEq n1 n2, PartialEq o1 o2)
    => PartialEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
