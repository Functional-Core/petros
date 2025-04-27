{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Petros.Eq.Eq
    ( Eq (..)
    , eqBy
    , eqWith
    , eqOn
    ) where

import GHC.Generics
import Petros.Eq.PartialEq
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified
import GHC.Records (HasField (..))
import Data.Void
import Data.Int
import GHC.Natural
import Data.Word

class (PartialEq a) => Eq a where
    (==) :: a -> a -> Bool
    default (==) :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    (==) x y = geq (from x) (from y)
    {-# INLINE (==) #-}

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)
    {-# INLINE (/=) #-}

infix 4 ==, /=

eqBy :: (a -> a -> Bool) -> a -> a -> Bool
eqBy f x y = f x y

eqWith :: Eq b => (a -> b) -> a -> a -> Bool
eqWith f x y = f x == f y

eqOn :: forall f r a. (HasField f r a, Eq a) => r -> r -> Bool
eqOn = eqWith (getField @f)

--------------------------------------------------------------------------

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
    x == y = liftPrelude2 (Prelude.==) x y

deriving via (FromPrelude Void) instance Eq Void
deriving via (FromPrelude ()) instance Eq ()
deriving via (FromPrelude Bool) instance Eq Bool
deriving via (FromPrelude Char) instance Eq Char

deriving via (FromPrelude Int) instance Eq Int
deriving via (FromPrelude Int8) instance Eq Int8
deriving via (FromPrelude Int16) instance Eq Int16
deriving via (FromPrelude Int32) instance Eq Int32
deriving via (FromPrelude Int64) instance Eq Int64

deriving via (FromPrelude Integer) instance Eq Integer
deriving via (FromPrelude Natural) instance Eq Natural

deriving via (FromPrelude Word) instance Eq Word
deriving via (FromPrelude Word8) instance Eq Word8
deriving via (FromPrelude Word16) instance Eq Word16
deriving via (FromPrelude Word32) instance Eq Word32
deriving via (FromPrelude Word64) instance Eq Word64

deriving via (FromPrelude Ordering) instance Eq Ordering

--------------------------------------------------------------------------

deriving anyclass instance (Eq a) => Eq (Maybe a)
deriving anyclass instance (Eq a) => Eq [a]
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
