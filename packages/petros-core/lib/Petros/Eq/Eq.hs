{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Eq.Eq
    ( HetEq (..)
    , Eq
    , (===)
    , (/==)
    ) where

import GHC.Generics
import Petros.Eq.PartialEq
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified

class (HetPartialEq a b) => HetEq a b where
    (==) :: a -> b -> Bool
    default (==) :: (Generic a, Generic b, GEq (Rep a) (Rep b)) => a -> b -> Bool
    (==) x y = geq (from x) (from y)
    {-# INLINE (==) #-}

    (/=) :: a -> b -> Bool
    (/=) x y = not (x == y)
    {-# INLINE (/=) #-}

type Eq a = HetEq a a

(===) :: (Eq a) => a -> a -> Bool
(===) = (==)
{-# INLINE (===) #-}

(/==) :: (Eq a) => a -> a -> Bool
(/==) = (/=)
{-# INLINE (/==) #-}

infix 4 ==, /=, ===, /==

--------------------------------------------------------------------------

class GEq f g where
    geq :: f a -> g a -> Bool

instance {-# OVERLAPPING #-} GEq V1 V1 where
    geq = undefined

instance GEq V1 g where
    geq = undefined

instance GEq f V1 where
    geq = undefined

instance {-# OVERLAPPING #-} GEq U1 U1 where
    geq _ _ = True

instance GEq U1 g where
    geq _ _ = False

instance GEq f U1 where
    geq _ _ = False

instance (GEq f1 f2, GEq f1 g2, GEq g1 f2, GEq g1 g2) => GEq (f1 :+: g1) (f2 :+: g2) where
    geq (L1 x) (L1 y) = geq x y
    geq (L1 x) (R1 y) = geq x y
    geq (R1 x) (L1 y) = geq x y
    geq (R1 x) (R1 y) = geq x y

instance (GEq f1 g1, GEq f2 g2) => GEq (f1 :*: f2) (g1 :*: g2) where
    geq (x1 :*: x2) (y1 :*: y2) = geq x1 y1 && geq x2 y2

instance (HetEq c d) => GEq (K1 i c) (K1 j d) where
    geq (K1 x) (K1 y) = x == y

instance (GEq f g) => GEq (M1 i t f) (M1 j u g) where
    geq (M1 x) (M1 y) = geq x y

--------------------------------------------------------------------------

instance (HetEq b a) => HetEq a (Flipped b) where
    (==) = liftFlipped (==)

--------------------------------------------------------------------------

instance (Prelude.Eq a) => HetEq a (FromPrelude a) where
    x == y = liftPrelude (x Prelude.==) y

instance (Prelude.Eq a) => HetEq (FromPrelude a) a where
    x == y = liftPrelude (Prelude.== y) x

instance (Prelude.Eq a) => HetEq (FromPrelude a) (FromPrelude a) where
    (==) = liftPrelude2 (Prelude.==)

deriving via (FromPrelude Ordering) instance HetEq Ordering Ordering

--------------------------------------------------------------------------

deriving anyclass instance (HetEq a b) => HetEq (Maybe a) (Maybe b)
deriving anyclass instance (HetEq a c, HetEq a d, HetEq b c, HetEq b d) => HetEq (Either a b) (Either c d)

deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2)
    => HetEq (a1, b1) (a2, b2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2)
    => HetEq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2)
    => HetEq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2)
    => HetEq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2)
    => HetEq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2, HetEq k1 k2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2, HetEq k1 k2, HetEq l1 l2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2, HetEq k1 k2, HetEq l1 l2, HetEq m1 m2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2, HetEq k1 k2, HetEq l1 l2, HetEq m1 m2, HetEq n1 n2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (HetEq a1 a2, HetEq b1 b2, HetEq c1 c2, HetEq d1 d2, HetEq e1 e2, HetEq f1 f2, HetEq g1 g2, HetEq h1 h2, HetEq i1 i2, HetEq j1 j2, HetEq k1 k2, HetEq l1 l2, HetEq m1 m2, HetEq n1 n2, HetEq o1 o2)
    => HetEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
