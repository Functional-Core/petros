{-# LANGUAGE Safe #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Petros.Ord.PartialOrd (
    PartialOrd (..),
    (<=>?)
) where

import Petros.Eq
import Prelude hiding (Ord (..), Eq (..))
import GHC.Generics
import Prelude qualified (Ord (..))
import Petros.Internal

class (PartialEq a) => PartialOrd a where
    cmpPartial :: a -> a -> Maybe Ordering
    default cmpPartial :: (Generic a, GPartialOrd (Rep a)) => a -> a -> Maybe Ordering
    cmpPartial x y = gcmpPartial (from x) (from y)

    (<.) :: a -> a -> Bool
    x <. y = case cmpPartial x y of
        Just LT -> True
        _ -> False

    (<=.) :: a -> a -> Bool
    x <=. y = case cmpPartial x y of
        Just LT -> True
        Just EQ -> True
        _ -> False

    (>.) :: a -> a -> Bool
    x >. y = case cmpPartial x y of
        Just GT -> True
        _ -> False

    (>=.) :: a -> a -> Bool
    x >=. y = case cmpPartial x y of
        Just GT -> True
        Just EQ -> True
        _ -> False

    (>/<) :: a -> a -> Bool
    x >/< y = case cmpPartial x y of
        Nothing -> True
        _ -> False

(<=>?) :: (PartialOrd a) => a -> a -> Maybe Ordering
(<=>?) = cmpPartial

infix 4 <., <=., >., >=., >/<, <=>?

-----------------------------------------------------------------

class GPartialOrd f where
    gcmpPartial :: (f a) -> (f a) -> Maybe Ordering

instance GPartialOrd V1 where
    gcmpPartial = undefined

instance GPartialOrd U1 where
    gcmpPartial _ _ = Just EQ

instance (GPartialOrd f, GPartialOrd g) => GPartialOrd (f :+: g) where
    gcmpPartial (L1 x) (L1 y) = gcmpPartial x y
    gcmpPartial (R1 x) (R1 y) = gcmpPartial x y
    gcmpPartial _ _ = Nothing

instance (GPartialOrd f, GPartialOrd g) => GPartialOrd (f :*: g) where
    gcmpPartial (x1 :*: y1) (x2 :*: y2) =
        if ordx == ordy || ordy == Just EQ
            then ordx
            else Nothing
        where
            ordx = gcmpPartial x1 x2
            ordy = gcmpPartial y1 y2

instance (PartialOrd c) => GPartialOrd (K1 i c) where
    gcmpPartial (K1 x) (K1 y) = cmpPartial x y

instance (GPartialOrd c) => GPartialOrd (M1 i j c) where
    gcmpPartial (M1 x) (M1 y) = gcmpPartial x y

-----------------------------------------------------------------

instance (Prelude.Ord a, PartialEq a) => PartialOrd (FromPrelude a) where
    cmpPartial x y = Just $ liftPrelude2 (Prelude.compare) x y
    {-# INLINE cmpPartial #-}

    (<.) = liftPrelude2 (Prelude.<)
    (<=.) = liftPrelude2 (Prelude.<=)
    (>.) = liftPrelude2 (Prelude.>)
    (>=.) = liftPrelude2 (Prelude.>=)
    {-# INLINE (<.) #-}
    {-# INLINE (<=.) #-}
    {-# INLINE (>.) #-}
    {-# INLINE (>=.) #-}
