{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Generators where

import Control.Monad (mapM)
import Control.Monad.Reader (MonadReader (..), lift)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Test.Class
import Test.Instance
import Test.Law
import Test.MonoType
import Test.Resolver
import Test.TestGen
import Test.Utils
import Prelude hiding (exp)

-- Given a valid params type candidate,
-- generate a spec containing tests for
-- each law.
genTypeSpec :: Class -> [MonoType] -> TestGen (Maybe Exp)
genTypeSpec c params0 = do
    env <- ask
    let params = map inner params0
    tests <- mapM (env.genPropertyTest params) c.laws
    let specName = pprintTypes params
    env.genSpec specName tests

-- Given an instance, generate a spec for
-- each set of valid candidate param types.
genInstanceSpecs :: Class -> Instance -> TestGen [Exp]
genInstanceSpecs c i = do
    paramss <- validCandidates i
    mes <- mapM (genTypeSpec c) paramss
    pure $ catMaybes mes

-- Given a class, generate a spec for each instance.
genClassSpec :: Class -> TestGen (Maybe Exp)
genClassSpec c = do
    env <- ask
    is <- lift $ findInstances c.name
    specs <- mapM (genInstanceSpecs c) is
    env.genSpec (classLabel c) (concat specs)

getClassOrError :: Name -> Q Class
getClassOrError className = do
    let label = nameBase className
    mc <- getClass className
    case mc of
        Nothing -> error $ "Failed to get class " <> label
        Just c -> pure c

runTestGenClass :: TestGenEnv -> Name -> Q Exp
runTestGenClass env className = do
    let label = nameBase className
    c <- getClassOrError className
    mexp <- runTestGen env (genClassSpec c)
    case mexp of
        Nothing -> error $ "Failed to generate tests for " <> label
        Just exp -> pure exp

----------------------------------------------------------------------

genTestQuickCheck :: [Type] -> Law -> Q Exp
genTestQuickCheck params l =
    pure $
        ParensE (applyTypes (VarE l.name) params)

genTestHSpec :: (Law -> Q Exp) -> Law -> Q Exp
genTestHSpec genBody l =
    [|prop $(stringE l.label) $(genBody l)|]

propGenQuickCheck :: [Type] -> Law -> TestGen Exp
propGenQuickCheck params l = lift $ genTestHSpec (genTestQuickCheck params) l

specGenHSpec :: String -> [Exp] -> TestGen (Maybe Exp)
specGenHSpec _ [] = pure Nothing
specGenHSpec label es0 = do
    let es1 = map pure es0
        exp = [|describe $(stringE label) $(doE (map noBindS es1))|]
    Just <$> lift exp
