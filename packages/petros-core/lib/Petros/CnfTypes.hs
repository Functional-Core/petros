{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

module Petros.CnfTypes where

import Fcf (Eval, Exp, type (<=<), Pure1, type (@@), Foldr, type (++), Uncurry)
import GHC.TypeLits
import Fcf.Class.Functor (FMap)
import Data.Kind (Type)
import Fcf.Data.List (ConcatMap)
import Prelude

data Var (l :: Symbol)
data Not (p :: k)
data Ands (ps :: [k])
data Ors (ps :: [k])

type family MoveNotIn (ps :: [Type]) :: [Type] where
    MoveNotIn ps = FMap (Nnf <=< Pure1 Not) @@ ps

data Nnf :: p -> Exp p
type instance Eval (Nnf (Var l)) = Var l
type instance Eval (Nnf (Ands ps)) = Ands (FMap Nnf @@ ps)
type instance Eval (Nnf (Ors ps)) = Ors (FMap Nnf @@ ps)
type instance Eval (Nnf (Not (Var l))) = Not (Var l)
type instance Eval (Nnf (Not (Not p))) = Nnf @@ p
type instance Eval (Nnf (Not (Ands ps))) = Ors (MoveNotIn ps)
type instance Eval (Nnf (Not (Ors ps))) = Ands (MoveNotIn ps)

type family UnwrapAndsOrs (p :: Type) :: Type where
    UnwrapAndsOrs (Ands '[p]) = p
    UnwrapAndsOrs (Ands ps) = Ands (FMap UnwrapSingletons @@ ps)
    UnwrapAndsOrs (Ors '[p]) = p
    UnwrapAndsOrs (Ors ps) = Ors (FMap UnwrapSingletons @@ ps)

data UnwrapSingletons :: p -> Exp p
type instance Eval (UnwrapSingletons (Var l)) = Var l
type instance Eval (UnwrapSingletons (Not p)) = Not (UnwrapSingletons @@ p)
type instance Eval (UnwrapSingletons (Ands ps)) = UnwrapAndsOrs (Ands ps)
type instance Eval (UnwrapSingletons (Ors ps)) = UnwrapAndsOrs (Ors ps)

type family FlattenAnd (p :: Type) :: [Type] where
    FlattenAnd (Ands qs) = qs
    FlattenAnd q = '[q]

data FlattenAnds_ :: p -> Exp [p]
type instance Eval (FlattenAnds_ p) = FlattenAnd p

data FlattenAnds :: p -> Exp p
type instance Eval (FlattenAnds (Var l)) = Var l
type instance Eval (FlattenAnds (Not p)) = Not (FlattenAnds @@ p)
type instance Eval (FlattenAnds (Ands ps)) = Ands (ConcatMap FlattenAnds_ @@ ps)
type instance Eval (FlattenAnds (Ors ps)) = Ors (FMap FlattenAnds @@ ps)

type family FlattenOr (p :: Type) :: [Type] where
    FlattenOr (Ors qs) = qs
    FlattenOr q = '[q]

data FlattenOrs_ :: p -> Exp [p]
type instance Eval (FlattenOrs_ p) = FlattenOr p

data FlattenOrs :: p -> Exp p
type instance Eval (FlattenOrs (Var l)) = Var l
type instance Eval (FlattenOrs (Not p)) = Not (FlattenOrs @@ p)
type instance Eval (FlattenOrs (Ands ps)) = Ands (FMap FlattenOrs @@ ps)
type instance Eval (FlattenOrs (Ors ps)) = Ors (ConcatMap FlattenOrs_ @@ ps)

data Normalise :: p -> Exp p
type instance Eval (Normalise (Var l)) = Var l
type instance Eval (Normalise (Not p)) = Not (Normalise @@ p)
type instance Eval (Normalise (Ands ps)) =
    UnwrapSingletons @@ (FlattenAnds @@ (Ands (FMap Normalise @@ ps)))
type instance Eval (Normalise (Ors ps)) =
    UnwrapSingletons @@ (FlattenOrs @@ (Ors (FMap Normalise @@ ps)))

type family ContainsAnd (ps :: [Type]) :: Bool where
    ContainsAnd '[] = 'False
    ContainsAnd (Ands _ ': _) = 'True
    ContainsAnd (_ ': ps) = ContainsAnd ps

data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons x xs) = x ': xs

data CartProdApplyElem :: [[a]] -> a -> Exp [[a]]
type instance Eval (CartProdApplyElem acc x) = FMap (Cons x) @@ acc

data CartProdApplyList :: [a] -> [[a]] -> Exp [[a]]
type instance Eval (CartProdApplyList xs acc) = ConcatMap (CartProdApplyElem acc) @@ xs

type CartProdInit = '[ '[] ]

data CartProd :: [[a]] -> Exp [[a]]
type instance Eval (CartProd xss) = Foldr CartProdApplyList CartProdInit @@ xss

type TestCartProd = CartProd @@ '[ '[1,2,3], '[4,5,6], '[7,8,9] ]

type family SplitAndsInner (p :: Type) (res :: ([[Type]], [Type])) :: ([[Type]], [Type]) where
    SplitAndsInner (Ands ps) '(ands, others) = '( ps ': ands, others )
    SplitAndsInner p '(ands, others) = '( ands, p ': others )

data SplitAndsInnerExp :: p -> ([[p]], [p]) -> Exp ([[p]], [p])
type instance Eval (SplitAndsInnerExp p res) = SplitAndsInner p res

type SplitAndsInit = '( '[], '[] )

data SplitAnds :: [p] -> Exp ([[p]], [p])
type instance Eval (SplitAnds ps) = Foldr SplitAndsInnerExp SplitAndsInit @@ ps

data DistributeMakeOr :: [p] -> [p] -> Exp p
type instance Eval (DistributeMakeOr others xs) = Ors (Eval (xs ++ others))

data DistributeContainsAnd :: [[p]] -> [p] -> Exp [p]
type instance Eval (DistributeContainsAnd ands others) =
    FMap (DistributeMakeOr others) @@ (CartProd @@ ands)

data Distribute :: Bool -> [p] -> Exp p
type instance Eval (Distribute 'True ps) = 
    Cnf_ @@ Ands (Uncurry DistributeContainsAnd @@ (SplitAnds @@ ps))
type instance Eval (Distribute 'False ps) = Ors ps

data Cnf_ :: p -> Exp p
type instance Eval (Cnf_ (Var l)) = Var l
type instance Eval (Cnf_ (Not p)) = Not p -- nnf ensures p is a var
type instance Eval (Cnf_ (Ands ps)) = Normalise @@ (Ands (FMap Cnf_ @@ ps))
type instance Eval (Cnf_ (Ors ps)) = Normalise @@ (Distribute (ContainsAnd ps) @@ (FMap Cnf_ @@ ps))

data Cnf :: p -> Exp p
type instance Eval (Cnf p) = Cnf_ @@ (Nnf @@ p)

type Test i o = (Cnf @@ i) ~ o

type Input1 = Var "x"
type Output1 = Var "x"
type Test1 = Test Input1 Output1

type Input2 = Not (Var "x")
type Output2 = Not (Var "x")
type Test2 = Test Input2 Output2

type Input3 = Ors '[Var "a", Not (Var "b")]
type Output3 = Ors '[Var "a", Not (Var "b")]
type Test3 = Test Input3 Output3

type Input4 = Ands [Ors [Var "a", Var "b"], Ors [Not (Var "c"), Var "d"]]
type Output4 = Ands [Ors [Var "a", Var "b"], Ors [Not (Var "c"), Var "d"]]
type Test4 = Test Input4 Output4

type Input5 = Not (Ands [Var "a", Var "b"])
type Output5 = Ors [Not (Var "a"), Not (Var "b")]
type Test5 = Test Input5 Output5

type Input6 = Ors [Var "a", Ands [Var "b", Var "c"]]
type Output6 = ()
type Test6 = Test Input6 Output6

type Input7 = Ors [Ands [Var "a", Var "b"], Var "c", Ands [Var "d", Var "e"]]
type Output7 = ()
type Test7 = Test Input7 Output7

type Input8 = Ors [Ands [Var "a", Ors [Var "x", Ands [Var "y", Var "z"]]], Var "b"]
type Output8 = ()
type Test8 = Test Input8 Output8

type Input9 = Ands [
    Ors [Var "a", Var "b"],
    Ors [Var "x", Var "b"],
    Ors [Var "y", Var "b"],
    Ors [Var "z", Var "b"]
    ]
type Output9 = ()
type Test9 = Test Input9 Output9

type X = Cnf @@ Input9

-- test1 :: Test1 => Int
-- test1 = 42
--
-- test2 :: Test2 => Int
-- test2 = 42
--
-- test3 :: Test3 => Int
-- test3 = 42
--
-- test4 :: Test4 => Int
-- test4 = 42
--
-- test5 :: Test5 => Int
-- test5 = 42
--
-- test6 :: Test6 => Int
-- test6 = 42
--
-- test7 :: Test7 => Int
-- test7 = 42

-- tests :: [Int]
-- tests =
--     [ test1
--     , test2
--     , test3
--     , test4
--     , test5
--     , test6
--     , test7
--     ]
