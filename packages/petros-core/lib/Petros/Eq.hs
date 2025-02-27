{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO:
-- Add laws
-- Add basic instances

module Petros.Eq
    ( PartialEq (..)
    , (~=)
    , (?=)
    , (/~=)
    , PartialHEq (..)
    , (~==)
    , (~==.)
    , (?==)
    , (?==.)
    , (/~==)
    , (/~==.)
    , Eq
    , (==)
    , (/=)
    , HEq
    , (===)
    , (===.)
    , (/==)
    , (/==.)
    ) where

import Data.Complex (Complex)
import Data.Functor.Identity
import GHC.Generics
import Prelude hiding (Eq (..))
import Prelude qualified (Eq (..))
import Data.Monoid
import Data.List.NonEmpty (NonEmpty)

class PartialEq a where
    eqPartial :: a -> a -> Bool
    default eqPartial :: (Generic a, GPartialEq (Rep a)) => a -> a -> Bool
    eqPartial x y = geqPartial (from x) (from y)
    {-# INLINE eqPartial #-}

    eqMaybe :: a -> a -> Maybe Bool
    default eqMaybe :: (Generic a, GPartialEq (Rep a)) => a -> a -> Maybe Bool
    eqMaybe x y = geqMaybe (from x) (from y)
    {-# INLINE eqMaybe #-}

    neqPartial :: a -> a -> Bool
    neqPartial x y = not (eqPartial x y)
    {-# INLINE neqPartial #-}

(~=) :: (PartialEq a) => a -> a -> Bool
(~=) = eqPartial
{-# INLINE (~=) #-}

(?=) :: (PartialEq a) => a -> a -> Maybe Bool
(?=) = eqMaybe
{-# INLINE (?=) #-}

(/~=) :: (PartialEq a) => a -> a -> Bool
(/~=) = neqPartial
{-# INLINE (/~=) #-}

class PartialHEq a b where
    heqPartial :: a -> b -> Bool
    default heqPartial :: (Generic a, Generic b, GPartialHeq (Rep a) (Rep b)) => a -> b -> Bool
    heqPartial x y = gheqPartial (from x) (from y)
    {-# INLINE heqPartial #-}

    heqMaybe :: a -> b -> Maybe Bool
    default heqMaybe :: (Generic a, Generic b, GPartialHeq (Rep a) (Rep b)) => a -> b -> Maybe Bool
    heqMaybe x y = gheqMaybe (from x) (from y)
    {-# INLINE heqMaybe #-}

    hneqPartial :: a -> b -> Bool
    hneqPartial x y = not (heqPartial x y)
    {-# INLINE hneqPartial #-}

heqPartial_ :: (PartialHEq a b) => b -> a -> Bool
heqPartial_ = flip heqPartial
{-# INLINE heqPartial_ #-}

heqMaybe_ :: (PartialHEq a b) => b -> a -> Maybe Bool
heqMaybe_ = flip heqMaybe
{-# INLINE heqMaybe_ #-}

hneqPartial_ :: (PartialHEq a b) => b -> a -> Bool
hneqPartial_ = flip hneqPartial
{-# INLINE hneqPartial_ #-}

(~==) :: (PartialHEq a b) => a -> b -> Bool
(~==) = heqPartial
{-# INLINE (~==) #-}

(~==.) :: (PartialHEq a b) => b -> a -> Bool
(~==.) = heqPartial_
{-# INLINE (~==.) #-}

(?==) :: (PartialHEq a b) => a -> b -> Maybe Bool
(?==) = heqMaybe
{-# INLINE (?==) #-}

(?==.) :: (PartialHEq a b) => b -> a -> Maybe Bool
(?==.) = heqMaybe_
{-# INLINE (?==.) #-}

(/~==) :: (PartialHEq a b) => a -> b -> Bool
(/~==) = hneqPartial
{-# INLINE (/~==) #-}

(/~==.) :: (PartialHEq a b) => b -> a -> Bool
(/~==.) = hneqPartial_
{-# INLINE (/~==.) #-}

class (PartialEq a) => Eq a where
    eq :: a -> a -> Bool
    default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    eq x y = geq (from x) (from y)
    {-# INLINE eq #-}

    neq :: a -> a -> Bool
    neq x y = not (eq x y)
    {-# INLINE neq #-}

(==) :: (Eq a) => a -> a -> Bool
(==) = eq
{-# INLINE (==) #-}

(/=) :: (Eq a) => a -> a -> Bool
(/=) = neq
{-# INLINE (/=) #-}

class (PartialHEq a b) => HEq a b where
    heq :: a -> b -> Bool
    default heq :: (Generic a, Generic b, GHEq (Rep a) (Rep b)) => a -> b -> Bool
    heq x y = gheq (from x) (from y)
    {-# INLINE heq #-}

    hneq :: a -> b -> Bool
    hneq x y = not (heq x y)
    {-# INLINE hneq #-}

heq_ :: (HEq a b) => b -> a -> Bool
heq_ = flip heq
{-# INLINE heq_ #-}

hneq_ :: (HEq a b) => b -> a -> Bool
hneq_ = flip hneq
{-# INLINE hneq_ #-}

(===) :: (HEq a b) => a -> b -> Bool
(===) = heq
{-# INLINE (===) #-}

(===.) :: (HEq a b) => b -> a -> Bool
(===.) = heq_
{-# INLINE (===.) #-}

(/==) :: (HEq a b) => a -> b -> Bool
(/==) = hneq
{-# INLINE (/==) #-}

(/==.) :: (HEq a b) => b -> a -> Bool
(/==.) = hneq_
{-# INLINE (/==.) #-}

------------------------- Generic Instances -------------------------

-- GPartialEq
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
    geqPartial (x1 :*: y1) (x2 :*: y2) = (geqPartial x1 x2) && (geqPartial y1 y2)
    geqMaybe (x1 :*: y1) (x2 :*: y2) = liftA2 (&&) (geqMaybe x1 x2) (geqMaybe y1 y2)

instance (PartialEq c) => GPartialEq (K1 i c) where
    geqPartial (K1 x) (K1 y) = eqPartial x y
    geqMaybe (K1 x) (K1 y) = eqMaybe x y

instance (GPartialEq f) => GPartialEq (M1 i t f) where
    geqPartial (M1 x) (M1 y) = geqPartial x y
    geqMaybe (M1 x) (M1 y) = geqMaybe x y

-- GPartialHeq
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
    gheqPartial (K1 x) (K1 y) = heqPartial x y
    gheqMaybe (K1 x) (K1 y) = heqMaybe x y

instance (GPartialHeq f g) => GPartialHeq (M1 i t f) (M1 j u g) where
    gheqPartial (M1 x) (M1 y) = gheqPartial x y
    gheqMaybe (M1 x) (M1 y) = gheqMaybe x y

-- Eq
class GEq f where
    geq :: f a -> f a -> Bool

instance GEq V1 where
    geq = undefined

instance GEq U1 where
    geq _ _ = True

instance (GEq f, GEq g, GHEq f g) => GEq (f :+: g) where
    geq (L1 x) (L1 y) = geq x y
    geq (L1 x) (R1 y) = gheq x y
    geq (R1 x) (L1 y) = gheq y x
    geq (R1 x) (R1 y) = geq x y

instance (GEq f, GEq g) => GEq (f :*: g) where
    geq (x1 :*: y1) (x2 :*: y2) = geq x1 x2 && geq y1 y2

instance (Eq c) => GEq (K1 i c) where
    geq (K1 x) (K1 y) = eq x y

instance (GEq f) => GEq (M1 i t f) where
    geq (M1 x) (M1 y) = geq x y

-- HEq
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

instance (GHEq f g, GEq f, GEq g) => GHEq (f :+: g) (f :+: g) where
    gheq (L1 x) (L1 y) = geq x y
    gheq (L1 x) (R1 y) = gheq x y
    gheq (R1 x) (L1 y) = gheq y x
    gheq (R1 x) (R1 y) = geq x y

instance (GHEq f1 g1, GHEq f2 g2) => GHEq (f1 :*: f2) (g1 :*: g2) where
    gheq (x1 :*: x2) (y1 :*: y2) = gheq x1 y1 && gheq x2 y2

instance (HEq c d) => GHEq (K1 i c) (K1 j d) where
    gheq (K1 x) (K1 y) = heq x y

instance (GHEq f g) => GHEq (M1 i t f) (M1 j u g) where
    gheq (M1 x) (M1 y) = gheq x y

------------------------- Higher order instances -------------------------

-------------
-- PartialEq
-------------

deriving newtype instance (PartialEq a) => PartialEq (Identity a)
deriving newtype instance (PartialEq a) => PartialEq (Product a)
deriving newtype instance (PartialEq a) => PartialEq (Sum a)

deriving anyclass instance (PartialEq a) => PartialEq [a]
deriving anyclass instance (PartialEq a) => PartialEq (NonEmpty a)
deriving anyclass instance (PartialEq a) => PartialEq (Maybe a)
deriving anyclass instance (PartialEq a, PartialEq b, PartialHEq a b) => PartialEq (Either a b)

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
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    )
    => PartialEq (a, b, c, d, e, f, g, h, i)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    , PartialEq n
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    , PartialEq n
    , PartialEq o
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

--------------
-- PartialHEq
--------------

instance (PartialEq a) => PartialHEq a a where
    heqPartial = eqPartial
    heqMaybe = eqMaybe

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

-------------
-- Eq
-------------

deriving anyclass instance (Eq a) => Eq (Maybe a)

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

-------------
-- HEq
-------------

instance (Eq a) => HEq a a where
    heq = eq

deriving anyclass instance (Eq a, Eq b, HEq a b) => Eq (Either a b)

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

--------------------------------------------------------------------------

newtype PreludeEq a = PreludeEq a
    deriving stock (Prelude.Eq)

instance (Prelude.Eq a) => PartialEq (PreludeEq a) where
    eqPartial = (Prelude.==)
    eqMaybe x y = Just $ (Prelude.==) x y

instance (Prelude.Eq a) => Eq (PreludeEq a) where
    eq = (Prelude.==)

-- deriving via (PreludeEq ByteArray) instance PartialEq ByteArray
-- deriving via (PreludeEq Timeout) instance PartialEq Timeout
-- deriving via (PreludeEq BigNat) instance PartialEq BigNat
-- deriving via (PreludeEq Void) instance PartialEq Void
-- deriving via (PreludeEq ByteOrder) instance PartialEq ByteOrder
-- deriving via (PreludeEq ClosureType) instance PartialEq ClosureType
-- deriving via (PreludeEq BlockReason) instance PartialEq BlockReason
-- deriving via (PreludeEq ThreadId) instance PartialEq ThreadId
-- deriving via (PreludeEq ThreadStatus) instance PartialEq ThreadStatus
-- deriving via (PreludeEq Constr) instance PartialEq Constr
-- deriving via (PreludeEq ConstrRep) instance PartialEq ConstrRep
-- deriving via (PreludeEq DataRep) instance PartialEq DataRep
-- deriving via (PreludeEq Fixity) instance PartialEq Fixity
-- deriving via (PreludeEq All) instance PartialEq All
-- deriving via (PreludeEq Any) instance PartialEq Any
-- deriving via (PreludeEq SomeTypeRep) instance PartialEq SomeTypeRep
-- deriving via (PreludeEq Unique) instance PartialEq Unique
-- deriving via (PreludeEq Version) instance PartialEq Version
-- deriving via (PreludeEq ControlMessage) instance PartialEq ControlMessage
-- deriving via (PreludeEq Event) instance PartialEq Event
-- deriving via (PreludeEq EventLifetime) instance PartialEq EventLifetime
-- deriving via (PreludeEq Lifetime) instance PartialEq Lifetime
-- deriving via (PreludeEq FdKey) instance PartialEq FdKey
-- deriving via (PreludeEq State) instance PartialEq State
-- deriving via (PreludeEq TimeoutKey) instance PartialEq TimeoutKey
-- deriving via (PreludeEq ErrorCall) instance PartialEq ErrorCall
-- deriving via (PreludeEq ArithException) instance PartialEq ArithException
-- deriving via (PreludeEq SpecConstrAnnotation) instance PartialEq SpecConstrAnnotation
-- deriving via (PreludeEq Fingerprint) instance PartialEq Fingerprint
-- deriving via (PreludeEq Errno) instance PartialEq Errno
-- deriving via (PreludeEq CBool) instance PartialEq CBool
-- deriving via (PreludeEq CChar) instance PartialEq CChar
-- deriving via (PreludeEq CClock) instance PartialEq CClock
-- deriving via (PreludeEq CDouble) instance PartialEq CDouble
-- deriving via (PreludeEq CFloat) instance PartialEq CFloat
-- deriving via (PreludeEq CInt) instance PartialEq CInt
-- deriving via (PreludeEq CIntMax) instance PartialEq CIntMax
-- deriving via (PreludeEq CIntPtr) instance PartialEq CIntPtr
-- deriving via (PreludeEq CLLong) instance PartialEq CLLong
-- deriving via (PreludeEq CLong) instance PartialEq CLong
-- deriving via (PreludeEq CPtrdiff) instance PartialEq CPtrdiff
-- deriving via (PreludeEq CSChar) instance PartialEq CSChar
-- deriving via (PreludeEq CSUSeconds) instance PartialEq CSUSeconds
-- deriving via (PreludeEq CShort) instance PartialEq CShort
-- deriving via (PreludeEq CSigAtomic) instance PartialEq CSigAtomic
-- deriving via (PreludeEq CSize) instance PartialEq CSize
-- deriving via (PreludeEq CTime) instance PartialEq CTime
-- deriving via (PreludeEq CUChar) instance PartialEq CUChar
-- deriving via (PreludeEq CUInt) instance PartialEq CUInt
-- deriving via (PreludeEq CUIntMax) instance PartialEq CUIntMax
-- deriving via (PreludeEq CUIntPtr) instance PartialEq CUIntPtr
-- deriving via (PreludeEq CULLong) instance PartialEq CULLong
-- deriving via (PreludeEq CULong) instance PartialEq CULong
-- deriving via (PreludeEq CUSeconds) instance PartialEq CUSeconds
-- deriving via (PreludeEq CUShort) instance PartialEq CUShort
-- deriving via (PreludeEq CWchar) instance PartialEq CWchar
-- deriving via (PreludeEq IntPtr) instance PartialEq IntPtr
-- deriving via (PreludeEq WordPtr) instance PartialEq WordPtr
-- deriving via (PreludeEq ForeignSrcLang) instance PartialEq ForeignSrcLang
-- deriving via (PreludeEq Associativity) instance PartialEq Associativity
-- deriving via (PreludeEq DecidedStrictness) instance PartialEq DecidedStrictness
-- deriving via (PreludeEq Fixity) instance PartialEq Fixity
-- deriving via (PreludeEq SourceStrictness) instance PartialEq SourceStrictness
-- deriving via (PreludeEq SourceUnpackedness) instance PartialEq SourceUnpackedness
-- deriving via (PreludeEq MaskingState) instance PartialEq MaskingState
-- deriving via (PreludeEq BufferState) instance PartialEq BufferState
-- deriving via (PreludeEq IODeviceType) instance PartialEq IODeviceType
-- deriving via (PreludeEq SeekMode) instance PartialEq SeekMode
-- deriving via (PreludeEq CodingProgress) instance PartialEq CodingProgress
-- deriving via (PreludeEq ArrayException) instance PartialEq ArrayException
-- deriving via (PreludeEq AsyncException) instance PartialEq AsyncException
-- deriving via (PreludeEq ExitCode) instance PartialEq ExitCode
-- deriving via (PreludeEq IOErrorType) instance PartialEq IOErrorType
-- deriving via (PreludeEq IOException) instance PartialEq IOException
-- deriving via (PreludeEq HandlePosn) instance PartialEq HandlePosn
-- deriving via (PreludeEq BufferMode) instance PartialEq BufferMode
-- deriving via (PreludeEq Handle) instance PartialEq Handle
-- deriving via (PreludeEq Newline) instance PartialEq Newline
-- deriving via (PreludeEq NewlineMode) instance PartialEq NewlineMode
-- deriving via (PreludeEq IOMode) instance PartialEq IOMode
-- deriving via (PreludeEq Int) instance PartialEq Int
-- deriving via (PreludeEq Word) instance PartialEq Word
-- deriving via (PreludeEq Integer) instance PartialEq Integer
-- deriving via (PreludeEq Natural) instance PartialEq Natural
-- deriving via (PreludeEq ()) instance PartialEq ()
-- deriving via (PreludeEq Bool) instance PartialEq Bool
-- deriving via (PreludeEq Char) instance PartialEq Char
-- deriving via (PreludeEq Double) instance PartialEq Double
-- deriving via (PreludeEq Float) instance PartialEq Float
