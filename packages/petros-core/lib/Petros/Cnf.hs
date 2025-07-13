module Petros.Cnf where

import Prelude

data Pred
    = Var Char
    | Not Pred
    | And [Pred]
    | Or [Pred]
    deriving stock (Show)

nnf :: Pred -> Pred
nnf (Var c) = Var c
nnf (And ps) = And (map nnf ps)
nnf (Or ps) = Or (map nnf ps)
nnf (Not (Var c)) = Not (Var c)
nnf (Not (Not p)) = nnf p
nnf (Not (And ps)) = And (map (nnf . Not) ps)
nnf (Not (Or ps)) = Or (map (nnf . Not) ps)

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
normalise (And ps) = unwrapSingletons $ flattenAnd $ And (map normalise ps)
normalise (Or ps) = unwrapSingletons $ flattenOr $ Or (map normalise ps)

containsAnd :: [Pred] -> Bool
containsAnd [] = False
containsAnd (And _ : _) = True
containsAnd (_ : ps) = containsAnd ps

cnf_ :: Pred -> Pred
cnf_ (Var c) = Var c
cnf_ (Not p) = Not p -- nnf ensures p is a var
cnf_ (And ps) = normalise $ And (map cnf_ ps)
cnf_ (Or ps) = normalise $ distribute $ map cnf_ ps
    where
        distribute qs
            | containsAnd qs = undefined
            | otherwise = Or qs

cnf :: Pred -> Pred
cnf = cnf_ . nnf
