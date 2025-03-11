{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Petros.Eq.Eq
    ( Eq (..)
    , Eq_
    , (===)
    , (/==)
    ) where

import GHC.Generics
import Petros.Eq.PartialEq
import Prelude hiding (Eq (..))
import Prelude qualified
import Petros.Internal

class (PartialEq a b) => Eq a b where
    (==) :: a -> b -> Bool
    default (==) :: (Generic a, Generic b, GEq (Rep a) (Rep b)) => a -> b -> Bool
    (==) x y = geq (from x) (from y)
    {-# INLINE (==) #-}

    (/=) :: a -> b -> Bool
    (/=) x y = not (x == y)
    {-# INLINE (/=) #-}

type Eq_ a = Eq a a

(===) :: Eq_ a => a -> a -> Bool
(===) = (==)
{-# INLINE (===) #-}

(/==) :: Eq_ a => a -> a -> Bool
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

instance (Eq c d) => GEq (K1 i c) (K1 j d) where
    geq (K1 x) (K1 y) = x == y

instance (GEq f g) => GEq (M1 i t f) (M1 j u g) where
    geq (M1 x) (M1 y) = geq x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => Eq a (FromPrelude a) where
    x == y = liftPrelude (x Prelude.==) y

instance (Prelude.Eq a) => Eq (FromPrelude a) a where
    x == y = liftPrelude (Prelude.== y) x
    
instance (Prelude.Eq a) => Eq (FromPrelude a) (FromPrelude a) where
    (==) = liftPrelude2 (Prelude.==)

deriving via (FromPrelude Ordering) instance Eq Ordering Ordering

--------------------------------------------------------------------------

deriving anyclass instance (Eq a b) => Eq (Maybe a) (Maybe b)
deriving anyclass instance (Eq a c, Eq a d, Eq b c, Eq b d) => Eq (Either a b) (Either c d)

deriving anyclass instance
    (Eq a1 a2, Eq b1 b2)
    => Eq (a1, b1) (a2, b2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2)
    => Eq (a1, b1, c1) (a2, b2, c2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2)
    => Eq (a1, b1, c1, d1) (a2, b2, c2, d2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2)
    => Eq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2)
    => Eq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2)
    => Eq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2, Eq k1 k2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2, Eq k1 k2, Eq l1 l2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2, Eq k1 k2, Eq l1 l2, Eq m1 m2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2, Eq k1 k2, Eq l1 l2, Eq m1 m2, Eq n1 n2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
deriving anyclass instance
    (Eq a1 a2, Eq b1 b2, Eq c1 c2, Eq d1 d2, Eq e1 e2, Eq f1 f2, Eq g1 g2, Eq h1 h2, Eq i1 i2, Eq j1 j2, Eq k1 k2, Eq l1 l2, Eq m1 m2, Eq n1 n2, Eq o1 o2)
    => Eq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
