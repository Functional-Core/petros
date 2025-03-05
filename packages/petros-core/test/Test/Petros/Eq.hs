{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Petros.Eq where

import Control.Monad (filterM, guard)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), lift)
import Data.List (isPrefixOf, intersperse, intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Language.Haskell.TH
import Petros.Eq
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)
import Prelude hiding (Eq (..), exp)
import Control.Category (Category)

{-
 - Class
 -  Name e.g. Eq
 -  Params e.g. a
 -  Laws
 -  Predicates e.g. PartialOrd a =>
 -  Instances
 -
 - Instances
 -  Type e.g. HEq Int a
 -  Params e.g. a
 -  Predicates e.g. (PartialOrd Int, Num a) =>
 -
 - Note that we only handle type constructors, applications and vars
 - We can separate an instance head into class type + params
 - We can try filling in any variable parameters
 - We can test those variables against the predicates in the instance
 - That leaves us with possible multiple sets of variables to complete
 - the instance head
 - We take our possibly multiple complete instance head param(s) :: [[Type]]
 - and submit them as the type parameter(s) against the class's own preds.
 - This must be recursive until a class with no preds is reached in order
 - to fully validate constraints.
 -
 -
 -}

-- ClassI (ClassD cxt nm tv fd ds) is
--
-- cxt = [AppT (ConT Petros.Eq.PartialEq.PartialEq) (VarT a_1)]
--
-- nm = Petros.Eq.Eq.Eq
--
-- tv = [KindedTV a_1 BndrReq StarT]
--
-- fd = []
--
-- ds = function declarations
-- 
-- instances ...
-- 
-- icxt0 = []
-- icxt1 = [AppT (ConT GHC.Classes.Eq) (VarT a_2)] 
--
-- t0 = AppT (ConT Petros.Eq.Eq.Eq) (ConT GHC.Types.Bool)
-- t1 = AppT (ConT Petros.Eq.Eq.Eq)
--      (AppT (ConT Petros.Internal.FromPrelude) (VarT a_2))
--
-- decs = function declarations
--
--
-- TODO: We need to test the predicates on the class too...
-- and recursively on superclasses?
testEq :: Q String
testEq = do 
    ClassI (ClassD cxt nm tv fd ds) is <- reify ''Eq
    let (InstanceD _ icxt t decs) = head $ drop 10 is
    pure $ intercalate "-----------" [show cxt, show tv, show t, show icxt]

testReify :: Q [InstanceDec]
testReify = reifyInstances ''Eq [AppT (ConT ''Maybe) (ConT ''Float)]

testCategory :: Q Dec
testCategory = do
    ClassI dec _ <- reify ''Category
    pure dec

testClass :: Name -> Q Dec
testClass n = do
    ClassI dec _ <- reify n
    pure dec

-- testDummy :: Q Dec
-- testDummy = do
--     ClassI dec _ <- reify ''Dummy
--     pure dec

-- class heads cannot have type application:
-- class Dummy (f a) where
--
-- but constraints can have type applications:
-- class (Eq (f a)) => Dummy f a where
--
-- class heads can however have kinds:
-- class Category (cat :: k -> k -> Type) where
-- 
-- this structure also occurs in seemingly "basic"
-- typeclasses such as Eq:
-- [KindedTV a_6989586621679059602 BndrReq StarT]
--
-- For now we will simply support standard type vars
-- by matching only on StarT. Any other kinds will
-- simply reject all candidates and thus generate
-- no tests.
--
-- These basic vars should be straightforward to
-- reify since when we submit candidate params to
-- a typeclass, that naturally binds them to the
-- positional vars such as f and a.
--
-- Let's explore the structure of constraints
-- typically they take a rather simple form of
-- TypeClass param0 ...
-- param0 could be var0 or (var1 var0) etc.
-- but we can handle either with ease.
--
-- We also see constraints of the form `a ~ _`
-- which we won't handle initially but should
-- be able to extend to handle without too much
-- of a problem.

data Class = Class
    { name :: Name
    , laws :: [Law]
    , instances :: [Instance]
    }
    deriving stock (Show)

data Law = Law
    { name :: Name
    }
    deriving stock (Show)

type ParamsType = Type

data Instance = Instance
    { name :: Name
    , params :: ParamsType
    , preds :: [Pred]
    }
    deriving stock (Show)

showClass :: Class -> String
showClass c = nameBase c.name

showLaw :: Law -> String
showLaw l = drop 4 $ nameBase l.name

showInst :: Instance -> String
showInst i = nameBase i.name

type PropertyGen = ParamsType -> Law -> TestGen Exp
type SpecGen = String -> [Exp] -> TestGen Exp

data TestGenEnv = TestGenEnv
    { typeCandidates :: [Type]
    , -- Such as Arbitrary and Show
      candidateInsts :: [Name]
    , -- how many successful candidates to test
      candidatesLimit :: Maybe Int
    , genPropertyTest :: PropertyGen
    , genSpec :: SpecGen
    }

mkCandidates :: [Name] -> [Type]
mkCandidates = map ConT

mkDefaultEnv :: PropertyGen -> SpecGen -> [Name] -> TestGenEnv
mkDefaultEnv pg sg insts =
    TestGenEnv
        { typeCandidates = mkCandidates [''Int, ''Bool, ''String]
        , candidatesLimit = Just 3
        , candidateInsts = insts
        , genPropertyTest = pg
        , genSpec = sg
        }

type TestGen a = ReaderT TestGenEnv Q a

runTestGen :: TestGenEnv -> TestGen a -> Q a
runTestGen env x = runReaderT x env

typeVars :: Type -> [Name]
typeVars (VarT n) = [n]
typeVars (AppT x y) = typeVars x <> typeVars y
typeVars _ = []

subVars :: Map Name Type -> Type -> Type
subVars vars (VarT n) = vars M.! n
subVars vars (AppT x y) = AppT (subVars vars x) (subVars vars y)
subVars _ t = t

data Candidate = Candidate
    { original :: Type
    , vars :: Map Name Type
    }
    deriving stock (Show)

varPermutations :: [Name] -> [Type] -> [Map Name Type]
varPermutations [] _ = []
varPermutations (v : vs) ts =
    let xs = varPermutations vs ts
     in [M.insert v t x | t <- ts, x <- xs]

candidates :: Type -> TestGen [Candidate]
candidates t = do
    env <- ask
    let vars = typeVars t
        vss = varPermutations vars env.typeCandidates
    pure $ map (Candidate t) vss

applyCandidate :: Candidate -> Type
applyCandidate c = subVars c.vars c.original

splitParams :: Type -> Maybe (Name, [Type])
splitParams (ConT n) = Just (n, [])
splitParams (AppT tx ty) = do
    (n, ts) <- splitParams tx
    pure (n, ts ++ [ty])
splitParams _ = Nothing

-- For now we only support preds of the form:
-- AppT (AppT ... (ConT predClass) param1) param2 ...)
-- all other predicates will be rejected.
checkPred :: Pred -> TestGen Bool
checkPred p = case splitParams p of
    Nothing -> pure False
    Just (n, ts) -> lift $ isInstance n ts

checkPreds :: Instance -> Candidate -> TestGen Bool
checkPreds i c = do
    let preds = map (subVars c.vars) i.preds
    results <- mapM checkPred preds
    pure $ and results

-- Is a Candidate valid for a given typeclass?
isValid :: Name -> Instance -> Candidate -> TestGen Bool
isValid className i c = do
    env <- ask
    let checks = className : env.candidateInsts
    results <- mapM checkInstance checks
    predsResult <- checkPreds i c
    pure $ and (predsResult : results)
    where
        checkInstance :: Name -> TestGen Bool
        checkInstance cName =
            lift $
                isInstance cName [applyCandidate c]

newtype ValidCandidate = Valid Candidate
    deriving stock (Show)

validCandidatesAll :: Name -> Instance -> TestGen [ValidCandidate]
validCandidatesAll className i = do
    cs0 <- candidates i.params
    cs1 <- filterM (isValid className i) cs0
    pure $ map Valid cs1

limitElems :: Maybe Int -> [a] -> [a]
limitElems Nothing xs = xs
limitElems (Just limit) xs = take limit xs

validCandidates :: Name -> Instance -> TestGen [ValidCandidate]
validCandidates className i = do
    env <- ask
    cs <- validCandidatesAll className i
    pure $ limitElems (env.candidatesLimit) cs

showType :: Type -> String
showType (AppT tx ty) = showType tx <> " " <> showType ty
showType (ConT name) = nameBase name
showType x = show x

showCandidate :: Candidate -> String
showCandidate c = showType $ applyCandidate c

findLaws :: Name -> Q [Law]
findLaws className = do
    ClassI (ClassD _ _ _ _ methods) _ <- reify className
    let laws = do
            SigD name _ <- methods
            guard ("law_" `isPrefixOf` (nameBase name))
            pure $ Law name
    pure laws

findInstances :: Name -> Q [Instance]
findInstances className = do
    ClassI _ is <- reify className
    -- FIXME: AppT _ params looks wrong based on the other nesting we've observed
    let pss = [(params, preds) | InstanceD _ preds (AppT _ params) _ <- is]
    pure $ map (uncurry $ Instance className) pss

-- Given a valid params type candidate,
-- generate a spec containing tests for
-- each law.
genTypeSpec :: Class -> ValidCandidate -> TestGen Exp
genTypeSpec cls (Valid cand) = do
    env <- ask
    tests <- mapM (env.genPropertyTest (applyCandidate cand)) cls.laws
    let specName = showCandidate cand
    env.genSpec specName tests

-- Given an instance generate a spec for
-- each set of valid candidate param types.
genInstanceSpecs :: Class -> Instance -> TestGen [Exp]
genInstanceSpecs c i = do
    cands <- validCandidates (c.name) i
    mapM (genTypeSpec c) cands

-- Given a class, generate a spec for each instance.
genClassSpec :: Class -> TestGen Exp
genClassSpec c = do
    env <- ask
    specs <- mapM (genInstanceSpecs c) c.instances
    env.genSpec (showClass c) (concat specs)

runTestGenClass :: TestGenEnv -> Name -> Q Exp
runTestGenClass env className = do
    laws <- findLaws className
    insts <- findInstances className
    let c = Class className laws insts
    runTestGen env (genClassSpec c)

----------------------------------------------------------------------

genTestQuickCheck :: Type -> Law -> Q Exp
genTestQuickCheck t l =
    pure $
        ParensE (AppTypeE (VarE l.name) t)

genTestHSpec :: (Law -> Q Exp) -> Law -> Q Exp
genTestHSpec genBody l =
    [|prop $(stringE (showLaw l)) $(genBody l)|]

propGenQuickCheck :: PropertyGen
propGenQuickCheck t l = lift $ genTestHSpec (genTestQuickCheck t) l

-- FIXME: Handle es0 == []
specGenHSpec :: SpecGen
specGenHSpec label es0 = do
    let es1 = map pure es0
    lift [|describe $(stringE label) $(doE (map noBindS es1))|]

testEnv :: TestGenEnv
testEnv =
    mkDefaultEnv
        propGenQuickCheck
        specGenHSpec
        [''Arbitrary, ''Show]

-- TODO: Collect all typeclasses if possible.
-- TODO: Emit warning if a typeclass/instance doesn't have tests.

-- eqSpec :: Spec
-- eqSpec = do
--     describe "Eq" do
--         describe "Int" do
--             prop "reflexivity" (law_reflexivity @Int)
