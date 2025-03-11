{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Petros.Ord.Strict
    ( Strict (..)
    , (<.)
    , (<=.)
    , (>.)
    , (>=.)
    , (>/<)
    , cmpPartial
    , StrictPartialOrd
    ) where

import GHC.Generics
import Petros.Eq
import Petros.Ord.PartialOrd (PartialOrd)
import Petros.Ord.PartialOrd qualified as Ord
import Prelude hiding (Eq (..), Ord (..))

--------------------------------------------------
-- Strict ordering
--------------------------------------------------

class GStrictPartialOrd f where
    gcmpStrictPartial :: (f a) -> (f a) -> Maybe Ordering

instance GStrictPartialOrd V1 where
    gcmpStrictPartial = undefined

instance GStrictPartialOrd U1 where
    gcmpStrictPartial _ _ = Just EQ

instance (GStrictPartialOrd f, GStrictPartialOrd g) => GStrictPartialOrd (f :+: g) where
    gcmpStrictPartial (L1 x) (L1 y) = gcmpStrictPartial x y
    gcmpStrictPartial (R1 x) (R1 y) = gcmpStrictPartial x y
    gcmpStrictPartial _ _ = Nothing

instance (GStrictPartialOrd f, GStrictPartialOrd g) => GStrictPartialOrd (f :*: g) where
    gcmpStrictPartial (x1 :*: y1) (x2 :*: y2) =
        if ordx == ordy
            then ordx
            else Nothing
        where
            ordx = gcmpStrictPartial x1 x2
            ordy = gcmpStrictPartial y1 y2

instance (PartialOrd c) => GStrictPartialOrd (K1 i c) where
    gcmpStrictPartial (K1 x) (K1 y) = Ord.cmpPartial x y

instance (GStrictPartialOrd c) => GStrictPartialOrd (M1 i j c) where
    gcmpStrictPartial (M1 x) (M1 y) = gcmpStrictPartial x y

newtype Strict a = Strict a
    deriving stock (Generic)
    deriving newtype (Show)
    deriving anyclass (PartialHEq (Strict a), HEq (Strict a))

instance (Generic a, PartialEq a, GStrictPartialOrd (Rep a)) => PartialOrd (Strict a) where
    cmpPartial (Strict x) (Strict y) = gcmpStrictPartial (from x) (from y)

type StrictPartialOrd a = (Generic a, PartialEq a, GStrictPartialOrd (Rep a))

liftStrict :: (Strict a -> Strict a -> b) -> a -> a -> b
liftStrict op x y = (Strict x) `op` (Strict y)
{-# INLINE liftStrict #-}

(<.), (<=.), (>.), (>=.), (>/<) :: (StrictPartialOrd a) => a -> a -> Bool
(<.) = liftStrict (Ord.<.)
(<=.) = liftStrict (Ord.<=.)
(>.) = liftStrict (Ord.>.)
(>=.) = liftStrict (Ord.>=.)
(>/<) = liftStrict (Ord.>/<)
{-# INLINE (<.) #-}
{-# INLINE (<=.) #-}
{-# INLINE (>.) #-}
{-# INLINE (>=.) #-}
{-# INLINE (>/<) #-}

cmpPartial :: (StrictPartialOrd a) => a -> a -> Maybe Ordering
cmpPartial = liftStrict Ord.cmpPartial
