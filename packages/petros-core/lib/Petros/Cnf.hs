{-# LANGUAGE UndecidableInstances #-}
module Petros.Cnf where

import Prelude

data Pred
    = Var Char
    | Not Pred
    | And [Pred]
    | Or [Pred]
    deriving stock (Show, Eq)

nnf :: Pred -> Pred
nnf (Var c) = Var c
nnf (And ps) = And (map nnf ps)
nnf (Or ps) = Or (map nnf ps)
nnf (Not (Var c)) = Not (Var c)
nnf (Not (Not p)) = nnf p
nnf (Not (And ps)) = Or (map (nnf . Not) ps)
nnf (Not (Or ps)) = And (map (nnf . Not) ps)

unwrapSingletons :: Pred -> Pred
unwrapSingletons (Var c) = Var c
unwrapSingletons (Not p) = Not (unwrapSingletons p)
unwrapSingletons (And [p]) = p
unwrapSingletons (Or [p]) = p
unwrapSingletons (And ps) = And (map unwrapSingletons ps)
unwrapSingletons (Or ps) = Or (map unwrapSingletons ps)

flattenAnd :: Pred -> Pred
flattenAnd (Var c) = Var c
flattenAnd (Not p) = Not (flattenAnd p)
flattenAnd (Or ps) = Or (map flattenAnd ps)
flattenAnd (And ps) = And (concatMap flattenAnd_ ps)
    where
        flattenAnd_ (And qs) = qs
        flattenAnd_ q = [q]

flattenOr :: Pred -> Pred
flattenOr (Var c) = Var c
flattenOr (Not p) = Not (flattenOr p)
flattenOr (And ps) = And (map flattenOr ps)
flattenOr (Or ps) = Or (concatMap flattenOr_ ps)
    where
        flattenOr_ (Or qs) = qs
        flattenOr_ q = [q]

normalise :: Pred -> Pred
normalise (Var c) = Var c
normalise (Not p) = Not (normalise p)
normalise (And ps) =
    unwrapSingletons
    $ flattenAnd
    $ And (map normalise ps)
normalise (Or ps) = unwrapSingletons
    $ flattenOr
    $ Or (map normalise ps)

containsAnd :: [Pred] -> Bool
containsAnd [] = False
containsAnd (And _ : _) = True
containsAnd (_ : ps) = containsAnd ps

cartProd :: [[a]] -> [[a]]
cartProd = foldr cartProdApplyList [[]]
    where
        cartProdApplyList xs acc = concatMap (cartProdApplyElem acc) xs
        cartProdApplyElem acc x = fmap (x :) acc

splitAnds :: [Pred] -> ([[Pred]], [Pred])
splitAnds = foldr something ([], [])
    where
        something (And qs) (ands, others) = (qs : ands, others)
        something p (ands, others) = (ands, p : others)

distributeMakeOr :: [Pred] -> [Pred] -> Pred
distributeMakeOr others xs = Or (xs ++ others)

distributeContainsAnd :: [[Pred]] -> [Pred] -> [Pred]
distributeContainsAnd ands others = fmap (distributeMakeOr others) (cartProd ands)

distribute :: [Pred] -> Pred
distribute ps =
    if containsAnd ps
        then cnf_ $ And $ uncurry distributeContainsAnd (splitAnds ps)
        else Or ps

cnf_ :: Pred -> Pred
cnf_ (Var c) = Var c
cnf_ (Not p) = Not p -- nnf ensures p is a var
cnf_ (And ps) = normalise $ And (map cnf_ ps)
cnf_ (Or ps) = normalise $ distribute $ map cnf_ ps

cnf :: Pred -> Pred
cnf = cnf_ . nnf

------------------------------------------------------------------------

-- Test 1: Simple Atom
input1 :: Pred
input1 = Var 'a'
cnf1 :: Pred
cnf1 = Var 'a'
test1 :: Bool
test1 = cnf input1 == cnf1

-- Test 2: Negated Atom
input2 :: Pred
input2 = Not (Var 'a')
cnf2 :: Pred
cnf2 = Not (Var 'a')
test2 :: Bool
test2 = cnf input2 == cnf2

-- Test 3: Disjunction of Literals
input3 :: Pred
input3 = Or [Var 'a', Not (Var 'b')]
cnf3 :: Pred
cnf3 = Or [Var 'a', Not (Var 'b')]
test3 :: Bool
test3 = cnf input3 == cnf3

-- Test 4: Conjunction of Disjunctions
input4 :: Pred
input4 = And [Or [Var 'a', Var 'b'], Or [Not (Var 'c'), Var 'd']]
cnf4 :: Pred
cnf4 = And [Or [Var 'a', Var 'b'], Or [Not (Var 'c'), Var 'd']]
test4 :: Bool
test4 = cnf input4 == cnf4

-- Test 5: Negated Conjunction (requires NNF)
input5 :: Pred
input5 = Not (And [Var 'a', Var 'b'])

cnf5 :: Pred
cnf5 = Or [Not (Var 'a'), Not (Var 'b')]

test5 :: Bool
test5 = cnf input5 == cnf5

-- Test 6: Distribution Required
input6 :: Pred
input6 = Or [Var 'a', And [Var 'b', Var 'c']]

cnf6 :: Pred
cnf6 = And [Or [Var 'a', Var 'b'], Or [Var 'a', Var 'c']]

test6 :: Bool
test6 = cnf input6 == cnf6

-- Test 7: Multiple Ands in Or
input7 :: Pred
input7 = Or [And [Var 'a', Var 'b'], Var 'c', And [Var 'd', Var 'e']]

cnf7 :: Pred
cnf7 = And
  [ Or [Var 'a', Var 'd', Var 'c']
  , Or [Var 'a', Var 'e', Var 'c']
  , Or [Var 'b', Var 'd', Var 'c']
  , Or [Var 'b', Var 'e', Var 'c']
  ]

test7 :: Bool
test7 = cnf input7 == cnf7

-- tests 6 and 7 technically fail right now due to ordering but the output is still correct
--
-- ordering won't be an issue in our real version since we'll be using sets
