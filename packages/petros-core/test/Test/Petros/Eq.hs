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
import Test.Law
import Test.Class
import Test.Instance
import Test.TestGen
import Test.Generators

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

-- Should these sorts of functions just take Class
-- as an argument for convenience?
-- We need to break the instance head into params.
-- It's always going to be of the form
-- AppT (AppT (ConT ClassName) param0) param1)
-- Note that those params could themselves contain
-- nested AppTs

-- we also need to scan the params for vars
-- if an instance head is concrete, we can
-- just generate tests for it, no need to
-- perform any other actions.
-- If the head is wrong, then that means it's
-- wrong in the code the user wrote so that's
-- not an issue we need to handle.

test = do
    TyConI (TySynD _ _ t) <- reify ''String
    c <- reifyInstances ''Arbitrary [t]
    pure c

testEnv :: TestGenEnv
testEnv =
    mkDefaultEnv
        propGenQuickCheck
        specGenHSpec
        [''Arbitrary, ''Show]

-- TODO: Collect all typeclasses if possible.
-- TODO: Emit warning if a typeclass/instance doesn't have tests.
