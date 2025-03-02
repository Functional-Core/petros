{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Petros.Eq.Eq
    ( Eq (..)
    ) where

import GHC.Generics
import Petros.Eq.PartialEq
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified

class (PartialEq a) => Eq a where
    (==) :: a -> a -> Bool
    default (==) :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    (==) x y = geq (from x) (from y)
    {-# INLINE (==) #-}

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)
    {-# INLINE (/=) #-}

class GEq f where
    geq :: f a -> f a -> Bool

instance GEq V1 where
    geq = undefined

instance GEq U1 where
    geq _ _ = True

instance (GEq f, GEq g) => GEq (f :+: g) where
    geq (L1 x) (L1 y) = geq x y
    geq (R1 x) (R1 y) = geq x y
    geq _ _ = False

instance (GEq f, GEq g) => GEq (f :*: g) where
    geq (x1 :*: y1) (x2 :*: y2) = geq x1 x2 && geq y1 y2

instance (Eq c) => GEq (K1 i c) where
    geq (K1 x) (K1 y) = x == y

instance (GEq f) => GEq (M1 i t f) where
    geq (M1 x) (M1 y) = geq x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => Eq (FromPrelude a) where
    (==) = liftPrelude2 (Prelude.==)

deriving via (FromPrelude Ordering) instance Eq Ordering

--------------------------------------------------------------------------

deriving anyclass instance (Eq a) => Eq (Maybe a)
deriving anyclass instance (Eq a, Eq b) => Eq (Either a b)

deriving anyclass instance (Eq a, Eq b) => Eq (a, b)
deriving anyclass instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
deriving anyclass instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
deriving anyclass instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
    => Eq (a, b, c, d, e, f)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
    => Eq (a, b, c, d, e, f, g)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h)
    => Eq (a, b, c, d, e, f, g, h)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i)
    => Eq (a, b, c, d, e, f, g, h, i)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j)
    => Eq (a, b, c, d, e, f, g, h, i, j)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k)
    => Eq (a, b, c, d, e, f, g, h, i, j, k)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l)
    => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
    => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
    => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving anyclass instance
    (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
    => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
