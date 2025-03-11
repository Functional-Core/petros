{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module Petros.Eq.Eq
    ( HEq (..)
    , Eq
    , (==)
    , (/=)
    ) where

import GHC.Generics
import Petros.Eq.PartialEq
import Prelude hiding (Eq (..))

class (PartialHEq a b) => HEq a b where
    (===) :: a -> b -> Bool
    default (===) :: (Generic a, Generic b, GHEq (Rep a) (Rep b)) => a -> b -> Bool
    (===) x y = gheq (from x) (from y)
    {-# INLINE (===) #-}

    (/==) :: a -> b -> Bool
    (/==) x y = not (x === y)
    {-# INLINE (/==) #-}

type Eq a = HEq a a

(==) :: Eq a => a -> a -> Bool
(==) = (===)
{-# INLINE (==) #-}

(/=) :: Eq a => a -> a -> Bool
(/=) = (/==)

--------------------------------------------------------------------------

class GHEq f g where
    gheq :: f a -> g a -> Bool

instance {-# OVERLAPPING #-} GHEq V1 V1 where
    gheq = undefined

instance GHEq V1 g where
    gheq = undefined

instance GHEq f V1 where
    gheq = undefined

instance {-# OVERLAPPING #-} GHEq U1 U1 where
    gheq _ _ = True

instance GHEq U1 g where
    gheq _ _ = False

instance GHEq f U1 where
    gheq _ _ = False

instance (GHEq f1 f2, GHEq f1 g2, GHEq g1 f2, GHEq g1 g2) => GHEq (f1 :+: g1) (f2 :+: g2) where
    gheq (L1 x) (L1 y) = gheq x y
    gheq (L1 x) (R1 y) = gheq x y
    gheq (R1 x) (L1 y) = gheq x y
    gheq (R1 x) (R1 y) = gheq x y

instance (GHEq f1 g1, GHEq f2 g2) => GHEq (f1 :*: f2) (g1 :*: g2) where
    gheq (x1 :*: x2) (y1 :*: y2) = gheq x1 y1 && gheq x2 y2

instance (HEq c d) => GHEq (K1 i c) (K1 j d) where
    gheq (K1 x) (K1 y) = x === y

instance (GHEq f g) => GHEq (M1 i t f) (M1 j u g) where
    gheq (M1 x) (M1 y) = gheq x y

--------------------------------------------------------------------------

deriving anyclass instance (HEq a c, HEq a d, HEq b c, HEq b d) => HEq (Either a b) (Either c d)

deriving anyclass instance
    (HEq a1 a2, HEq b1 b2)
    => HEq (a1, b1) (a2, b2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2)
    => HEq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2)
    => HEq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2)
    => HEq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2)
    => HEq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2)
    => HEq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2, HEq k1 k2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2, HEq k1 k2, HEq l1 l2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2, HEq k1 k2, HEq l1 l2, HEq m1 m2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2, HEq k1 k2, HEq l1 l2, HEq m1 m2, HEq n1 n2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (HEq a1 a2, HEq b1 b2, HEq c1 c2, HEq d1 d2, HEq e1 e2, HEq f1 f2, HEq g1 g2, HEq h1 h2, HEq i1 i2, HEq j1 j2, HEq k1 k2, HEq l1 l2, HEq m1 m2, HEq n1 n2, HEq o1 o2)
    => HEq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
