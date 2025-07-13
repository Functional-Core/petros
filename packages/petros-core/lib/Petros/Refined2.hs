{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

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

-- Predicate definitions

data Var (s :: Symbol)
data Not (p :: k)
data Ors (ps :: [k])
data Ands (ps :: [k])

type Or p q = Ors [p, q]
type And p q = Ands [p, q]

------------------------------------------------------------

type family Map (f :: kx -> ky) (xs :: [kx]) :: [ky] where
    Map _ '[] = '[]
    Map f (x ': xs) = (f x) ': Map f xs

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    xs ++ '[] = xs
    (x ': xs) ++ ys = x ': (xs ++ ys)

type family NNF (p :: Type) :: Type where
    NNF (Not (Not p)) = NNF p
    NNF (Not (Ors ps)) = Ands (Map Not ps)
    NNF (Not (Ands ps)) = Ors (Map Not ps)
    NNF (Not p) = Not (NNF p)
    NNF (Ands ps) = Ands (MapNnf ps)
    NNF (Ors ps) = Ors (MapNnf ps)
    NNF (Var s) = Var s

type family MapNnf (ps :: [Type]) :: [Type] where
    MapNnf '[] = '[]
    MapNnf (p ': ps) = NNF p ': MapNnf ps

type family FlattenAnds (ps :: [Type]) :: [Type] where
    FlattenAnds '[] = '[]
    FlattenAnds (Ands qs ': ps) = qs ++ FlattenAnds ps
    FlattenAnds (p ': ps) = p ': FlattenAnds ps

type family FlattenOrs (ps :: [Type]) :: [Type] where
    FlattenOrs '[] = '[]
    FlattenOrs (Ors qs ': ps) = qs ++ FlattenOrs ps
    FlattenOrs (p ': ps) = p ': FlattenOrs ps

type family ContainsAnds (ps :: [Type]) :: Bool where
    ContainsAnds '[] = 'False
    ContainsAnds (Ands _ ': _) = 'True
    ContainsAnds (_ ': ps) = ContainsAnds ps

type family IfThenElse (b :: Bool) (t :: k) (f :: k) :: k where
    IfThenElse 'True t _ = t
    IfThenElse 'False _ f = f

-- Called on the list of predicates within an `Ands`.
type family DistributePred (as :: [Type]) (ps :: [Type]) :: [Type] where
    DistributePred '[] _ = '[]
    DistributePred (a ': as) ps = CNF (Ors (a ': ps)) ': DistributePred as ps

type family MapDistributePred (ps :: [Type]) (prevPs :: [Type]) (nextPs :: [Type]) :: [Type] where
    MapDistributePred '[] _ _ = '[]
    MapDistributePred (Ands as ': ps) prevPs (this ': nextPs) =
        DistributePred as (prevPs ++ nextPs) ++ MapDistributePred ps (this ': prevPs) nextPs
    MapDistributePred (p ': ps) prevPs (_ ': nextPs) = MapDistributePred ps (p ': prevPs) nextPs

type family MapDistributePredIfAnds (ps :: [Type]) :: Type where
    MapDistributePredIfAnds ps =
        IfThenElse
            (ContainsAnds ps)
            (Ands (MapDistributePred ps '[] ps))
            (Ors ps)

type family NnfToCnf (p :: Type) :: Type where
    NnfToCnf (Ands ps) = Ands (FlattenAnds (MapCnf ps))
    NnfToCnf (Ors ps) = MapDistributePredIfAnds (FlattenOrs (MapCnf ps))
    NnfToCnf p = p

type family MapCnf (ps :: [Type]) :: [Type] where
    MapCnf '[] = '[]
    MapCnf (p ': ps) = NnfToCnf p ': MapCnf ps

type family CNF (p :: Type) :: Type where
    CNF p = NnfToCnf (NNF p)

-- TODO:
-- need to implement type instance Compare for (Or)

------------------------------------------------------------

-- Test Cases

-- 1. Simple atom
type Test1 = CNF (Var "x") 
-- Should be: Var "x"

-- 2. Simple Not
type Test2 = CNF (Not (Var "x"))
-- Should be: Not (Var "x")

-- 3. Double negation
type Test3 = CNF (Not (Not (Var "x")))
-- Should be: Var "x"

-- 4. Or of two atoms
type Test4 = CNF (Ors '[Var "x", Var "y"])
-- Should be: Ors '[Var "x", Var "y"]

-- 5. And of two atoms
type Test5 = CNF (Ands '[Var "x", Var "y"])
-- Should be: Ands '[Var "x", Var "y"]

-- 6. Or of And
type Test6 = CNF (Ors '[Ands '[Var "x", Var "y"], Var "z"])
-- Should be: Ands '[Ors '[Var "x", Var "z"], Ors '[Var "y", Var "z"]]

-- 7. Or of nested And
type Test7 = CNF (Ors '[Ands '[Var "x", Var "y"], Ands '[Var "a", Var "b"]])
-- Should be: Ands '[Ors '[Var "x", Ands '[Var "a", Var "b"]], Ors '[Var "y", Ands '[Var "a", Var "b"]]]
--
-- distribute
--
-- Ands
--  '[ Ors '[Var "x", Ands '[Var "a", Var "b"]]
--   , Ors '[Var "y", Ands '[Var "a", Var "b"]]
--   , Ors '[Var "a", Ands '[Var "x", Var "y"]]
--   , Ors '[Var "b", Ands '[Var "x", Var "y"]]
--   ]
--
-- distribute again
--
-- Ands
--  '[ Ands
--      '[ Ors '[Var "a", Var "x"]
--       , Ors '[Var "b", Var "x"]
--       ]
--   , Ands
--      '[ Ors '[Var "a", Var "y"]
--       , Ors '[Var "b", Var "y"]
--       ]
--   , Ands
--      '[ Ors '[Var "x", Var "a"]
--       , Ors '[Var "y", Var "a"]
--       ]
--   , Ands
--      '[ Ors '[Var "x", Var "b"]
--       , Ors '[Var "y", Var "b"]
--       ]
--   ]
--
-- flatten
--
-- Ands
--  '[ Ors '[Var "a", Var "x"]
--   , Ors '[Var "b", Var "x"]
--   , Ors '[Var "a", Var "y"]
--   , Ors '[Var "b", Var "y"]
--   , Ors '[Var "x", Var "a"]
--   , Ors '[Var "y", Var "a"]
--   , Ors '[Var "x", Var "b"]
--   , Ors '[Var "y", Var "b"]
--   ]
--
-- we now have a bunch of duplicates since x `or` y ~ y `or` x
--
-- Ands
--  '[ Ors '[Var "a", Var "x"]
--   , Ors '[Var "b", Var "x"]
--   , Ors '[Var "a", Var "y"]
--   , Ors '[Var "b", Var "y"]
--   ]

-- Is it true that
--  Ors [ Ands [ a, b ], x, Ands [ c, d ], y ]
-- is equivalent to
--  Ors [ Ands [ a, b, c, d ], x, y ]
--
-- If it is, then we should join adjacent ands together (likewise with ors?)
-- to prevent the above duplication.
--
-- No it's not...

-- 8. Nested Ors and Ands
type Test8 = CNF (Ors '[Var "a", Ands '[Var "b", Ors '[Var "c", Var "d"]]])
-- Should be: Ands '[Ors '[Var "a", Var "b"], Ors '[Var "a", Ors '[Var "c", Var "d"]]]

-- 9. Deeply nested
type Test9 = CNF (Ors '[Var "a", Ands '[Ors '[Var "b", Var "c"], Var "d"]])
-- Should be: Ands '[Ors '[Var "a", Ors '[Var "b", Var "c"]], Ors '[Var "a", Var "d"]]

-- 7, 8 and 9 don't look right because they have nested Ors, need to check what's correct.

type Proof1 = Test1 ~ Var "x"
type Proof2 = Test2 ~ Not (Var "x")
type Proof3 = Test3 ~ Var "x"
type Proof4 = Test4 ~ Ors '[Var "x", Var "y"]
type Proof5 = Test5 ~ Ands '[Var "x", Var "y"]
type Proof6 = Test6 ~ Ands '[Ors '[Var "x", Var "z"], Ors '[Var "y", Var "z"]]
type Proof7 = Test7 ~ Ands '[Ors '[Var "a", Var "x"], Ors '[Var "b", Var "x"], Ors '[Var "a", Var "y"], Ors '[Var "b", Var "y"]]
-- type Proof7 = Test7 ~ Ands '[Ors '[Var "x", Ands '[Var "a", Var "b"]], Ors '[Var "y", Ands '[Var "a", Var "b"]]]
type Proof8 = Test8 ~ Ands '[Ors '[Var "a", Var "b"], Ors '[Var "a", Ors '[Var "c", Var "d"]]]
type Proof9 = Test9 ~ Ands '[Ors '[Var "a", Ors '[Var "b", Var "c"]], Ors '[Var "a", Var "d"]]

proof1 :: Proof1 => Int
proof1 = 1

proof2 :: Proof2 => Int
proof2 = 2

proof3 :: Proof3 => Int
proof3 = 3

proof4 :: Proof4 => Int
proof4 = 4

proof5 :: Proof5 => Int
proof5 = 5

proof6 :: Proof6 => Int
proof6 = 6

proof7 :: Proof7 => Int
proof7 = 7

proof8 :: Proof8 => Int
proof8 = 8

proof9 :: Proof9 => Int
proof9 = 9

tests :: [Int]
tests =
    [ proof1
    , proof2
    , proof3
    , proof4
    , proof5
    , proof6
    -- , proof7
    -- , proof8
    -- , proof9
    ]
