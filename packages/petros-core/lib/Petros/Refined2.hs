{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}

module Petros.Refined2 where

import Data.Kind
import Data.Type.Ord
import GHC.TypeLits
import Prelude


type family Nub (xs :: [k]) :: [k] where
    Nub xs = Nub_ xs '[]

type family Nub_ (xs :: [k]) (acc :: [k]) :: [k] where
    Nub_ '[] acc = acc
    Nub_ (x ': xs) acc = Insert x (Nub_ xs acc)

type family Insert (x :: k) (xs :: [k]) :: [k] where
    Insert x '[] = '[x]
    Insert x (x ': xs) = x ': xs
    Insert x (y ': xs) = y ': Insert x xs

type family Sort (xs :: [k]) :: [k] where
    Sort xs = Sort_ xs '[]

type family Sort_ (xs :: [k]) (acc :: [k]) :: [k] where
    Sort_ '[] acc = acc
    Sort_ (x ': xs) '[] = Sort_ xs '[x]
    Sort_ (x ': xs) (y ': ys) =
        OrdCond
            (Compare x y)
            (Sort_ xs (x ': y ': ys)) -- lt
            (Sort_ xs (x ': y ': ys)) -- eq
            (y ': Sort_ (x ': xs) ys) -- gt

type family Set (xs :: [k]) :: [k] where
    Set xs = Sort (Nub xs)

------------------------------------------------------------

class Predicate (p :: Type) a where
    type Label p :: Symbol
    check :: a -> Bool

data Positive

instance (Num a, Ord a) => Predicate Positive a where
    type Label Positive = "Positive"
    check n = n > 0

------------------------------------------------------------

-- type data CNF xs = Conjunction (Set xs)
-- type data Disjunction xs = Atom Symbol | Or (Set xs)

data Or xs = Or xs

-- TODO:
-- need to implement type instance Compare for (Or)
