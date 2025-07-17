{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

module Petros.Refined2 where

import Data.Kind
import Data.Type.Ord
import GHC.TypeLits
import Prelude
import Fcf (Eval, Exp, type (<=<), Pure1, type (@@), Foldr, type (++), Uncurry)
import Fcf.Class.Functor (FMap)
import Fcf.Data.List (ConcatMap)
import Data.Proxy (Proxy(..))
import Fcf.Class.Monoid (type (<>))
import Data.Type.Bool (type (&&))

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

-- Predicate definitions

data Var (p :: k)
data Not (p :: k)
data Ors (ps :: [k])
data Ands (ps :: [k])

type Or p q = Ors [p, q]
type And p q = Ands [p, q]

------------------------------------------------------------

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

type CNF p = Eval (Cnf p)

------------------------------------------------------------

type family ShowNat (n :: Nat) :: Symbol where
    ShowNat 0 = "0"
    ShowNat 1 = "1"
    ShowNat 2 = "2"
    ShowNat 3 = "3"
    ShowNat 4 = "4"
    ShowNat 5 = "5"
    ShowNat 6 = "6"
    ShowNat 7 = "7"
    ShowNat 8 = "8"
    ShowNat 9 = "9"
    ShowNat n = ShowNat (Div n 10) <> ShowNat (Mod n 10)

type family CmpElem (ord :: Ordering) (xs :: [k]) (ys :: [k]) :: Ordering where
    CmpElem 'EQ xs ys = CmpList xs ys
    CmpElem ord _ _ = ord

type family CmpList (xs :: [k]) (ys :: [k]) :: Ordering where
    CmpList '[] '[] = 'EQ
    CmpList '[] _ = 'LT
    CmpList _ '[] = 'GT
    CmpList (x ': xs) (y ': ys) = CmpElem (Compare x y) xs ys

-- Instances are incomplete and heavily influenced by already
-- being in CNF.
type instance Compare (Var x) (Var y) = CmpSymbol (Label x) (Label y)

type instance Compare (Var x) (Not (Var y)) =
    CmpSymbol (Label x) (Label (Not (Var y)))
type instance Compare (Not (Var x)) (Var y) =
    CmpSymbol (Label (Not (Var x))) (Label y)
type instance Compare (Not (Var x)) (Not (Var y)) =
    CmpSymbol (Label (Not (Var x))) (Label (Not (Var y)))

type instance Compare (Ors _) (Var _) = 'GT
type instance Compare (Ors _) (Not _) = 'GT
type instance Compare (Var _) (Ors _) = 'LT
type instance Compare (Not _) (Ors _) = 'LT
type instance Compare (Ors ps) (Ors qs) = CmpList ps qs

type instance Compare (Ands ps) (Ands qs) = CmpList ps qs

type family Setify (p :: Type) :: Type where
    Setify (Ands ps) = Ands (Set ps)
    Setify (Ors ps) = Ors (Set ps)

data Refined (p :: k) a = Refined { unrefine :: a }
    deriving stock (Show, Eq)

class RefinementPredicate a (p :: Type) where
    type Label p :: Symbol
    check :: a -> Bool 

refine :: forall p a. RefinementPredicate a p => a -> Maybe (Refined (Setify (CNF p)) a)
refine x
    | check @_ @p x = Just $ Refined x
    | otherwise = Nothing

unsafeRefine :: a -> Refined (Setify (CNF p)) a
unsafeRefine x = Refined x

data Positive

instance (Num a, Ord a) => RefinementPredicate a Positive where
    type Label Positive = "Positive"
    check n = n > 0

data LessThan (n :: Nat)

instance (Num a, Ord a, KnownNat n) => RefinementPredicate a (LessThan n) where
    type Label (LessThan n) = "LessThan_" <> ShowNat n
    check x = x < (fromIntegral $ natVal $ Proxy @n)

type SmallPositive = Ands '[Var Positive, Var (LessThan 10)]

instance (RefinementPredicate a p) => RefinementPredicate a (Var p) where
    type Label (Var p) = Label p
    check x = check @_ @p x

instance (RefinementPredicate a p) => RefinementPredicate a (Not (Var p)) where
    type Label (Not (Var p)) = "Not_" <> Label p
    check x = not $ check @_ @p x

instance RefinementPredicate a (Ands '[]) where
    type Label (Ands _) = "Ands"
    check _ = True

instance (RefinementPredicate a p, RefinementPredicate a (Ands ps))
    => RefinementPredicate a (Ands (p ': ps)) where
    type Label (Ands _) = "Ands"
    check x = check @_ @p x && check @_ @(Ands ps) x

instance RefinementPredicate a (Ors '[]) where
    type Label (Ors _) = "Ors"
    check _ = False

instance (RefinementPredicate a p, RefinementPredicate a (Ors ps))
    => RefinementPredicate a (Ors (p ': ps)) where
    type Label (Ors _) = "Ors"
    check x = check @_ @p x || check @_ @(Ors ps) x

-- if something is P it is automatically P OR Q.
-- if something is P AND Q it is automatically just P and just Q.

type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem _ '[] = 'False
    Elem x (x ': ys) = 'True
    Elem x (_ ': ys) = Elem x ys

type family Subset (xs :: [k]) (ys :: [k]) :: Bool where
    Subset '[] _ = 'True
    Subset (x ': xs) ys = Elem x ys && Subset xs ys

type family Is (b :: Bool) (e :: ErrorMessage) :: Constraint where
    Is 'True _ = ()
    Is 'False e = TypeError e

type family IsElem (x :: k) (xs :: [k]) :: Constraint where
    IsElem x xs = Is (Elem x xs) (Text "The type " :<>: ShowType x
        :<>: Text " is not a member of the type-level list " :<>: ShowType xs)

type family IsSubset (xs :: [k]) (ys :: [k]) :: Constraint where
    IsSubset xs ys = Is (Subset xs ys) (Text "The type-level list " :<>: ShowType xs
        :<>: Text " is not a subset of the type-level list " :<>: ShowType ys )

-- TODO: Is there a way we can stop nonsense instances of this being created?
-- Maybe we only export the `weaken` function?
class (RefinementPredicate a strong, RefinementPredicate a weak) => Weaken a strong weak where
    weaken :: Refined strong a -> Refined weak a
    weaken = Refined . unrefine

instance
    ( RefinementPredicate a p
    , RefinementPredicate a (Ors ps)
    , IsElem p ps
    ) => Weaken a p (Ors ps)

instance
    ( RefinementPredicate a p
    , RefinementPredicate a (Ands ps)
    , IsElem p ps
    ) => Weaken a (Ands ps) p

instance 
    ( RefinementPredicate a (Ors ps)
    , RefinementPredicate a (Ors qs)
    , IsSubset ps qs
    ) => Weaken a (Ors ps) (Ors qs)

instance
    ( RefinementPredicate a (Ands ps)
    , RefinementPredicate a (Ands qs)
    , IsSubset ps qs
    ) => Weaken a (Ands qs) (Ands ps)
