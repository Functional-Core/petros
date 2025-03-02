{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}

module Petros.Ord.Ord
    ( Ord (..)
    , (<=>)
    ) where

import GHC.Generics
import Petros.Eq
import Prelude hiding (Eq (..), Ord (..))
import Petros.Ord.PartialOrd
import Petros.Ord.PartialOrd ()
import Prelude qualified
import Petros.Internal

class (Eq a, PartialOrd a) => Ord a where
    cmp :: a -> a -> Ordering
    cmp x y =
        if x == y
            then EQ
            else
                if x <= y
                    then LT
                    else GT

    (<=) :: a -> a -> Bool
    x <= y = case cmp x y of
        GT -> False
        _ -> True

    (>=) :: a -> a -> Bool
    x >= y = y <= x

    (>) :: a -> a -> Bool
    x > y = not (x <= y)

    (<) :: a -> a -> Bool
    x < y = not (x >= y)

    min :: a -> a -> a
    min x y = if x <= y then x else y

    max :: a -> a -> a
    max x y = if x <= y then y else x

    {-# MINIMAL cmp | (<=) #-}

(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = cmp

class GOrd f where
    gcmp :: (f a) -> (f a) -> Ordering

instance GOrd V1 where
    gcmp = undefined

instance GOrd U1 where
    gcmp _ _ = EQ

-- There is no instance for (:+:) or (:*:) due
-- to there always being incomparable cases.

instance (Ord c) => GOrd (K1 i c) where
    gcmp (K1 x) (K1 y) = cmp x y

instance (GOrd c) => GOrd (M1 i j c) where
    gcmp (M1 x) (M1 y) = gcmp x y

instance (Prelude.Ord a, Eq a, PartialOrd a) => Ord (FromPrelude a) where
    cmp = liftPrelude2 Prelude.compare


