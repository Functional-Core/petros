{-# LANGUAGE UndecidableInstances #-}
module Petros.Experimental.TypeLevel where

import Prelude
import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))

-- class Eval l t | l -> t where
--     eval :: l -> t
--
-- data ListToMaybe a = ListToMaybe [a]
--
-- instance Eval (ListToMaybe a) (Maybe a) where
--     eval (ListToMaybe []) = Nothing
--     eval (ListToMaybe (x:_)) = Just x
    
type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (x ': xs)) = Eval (f x) ': Eval (MapList f xs)

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr _ acc '[]) = acc
type instance Eval (Foldr f acc (x ': xs)) = Eval (Foldr f (Eval (f x acc)) xs)

data Add :: Nat -> Nat -> Exp Nat
type instance Eval (Add x y) = x + y

type Test = Eval (Foldr Add 0 '[1,2,3])
