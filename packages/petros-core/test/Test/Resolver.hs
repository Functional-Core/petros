{-# LANGUAGE OverloadedRecordDot #-}

module Test.Resolver where

import Control.Monad.Reader (MonadReader (..), lift)
import Data.Map (Map)
import Data.Map qualified as M
import Language.Haskell.TH
import Test.Class
import Test.Instance
import Test.MonoType
import Test.TestGen
import Prelude hiding (pred)
import Data.List (nub)
import Test.Utils (limitElems)
import Control.Monad (filterM)

typeVars :: Type -> [Name]
typeVars (VarT n) = [n]
typeVars (AppT x y) = typeVars x <> typeVars y
typeVars _ = []

zipVars :: Type -> MonoType -> Map Name MonoType
zipVars (VarT n) t = M.fromList [(n, t)]
zipVars (AppT tx1 tx2) t =
    bimapMonoType (zipVars tx1) (zipVars tx2) t
zipVars _ _ = M.empty

varPermutations :: [Name] -> [MonoType] -> [Map Name MonoType]
varPermutations [] _ = []
varPermutations [v] ts = [M.fromList [(v, t)] | t <- ts]
varPermutations (v : vs) ts =
    let xs = varPermutations vs ts
     in [M.insert v t x | t <- ts, x <- xs]

splitPred :: Type -> Maybe (Name, [Type])
splitPred (ConT n) = Just (n, [])
splitPred (AppT tx ty) = do
    (n, ts) <- splitPred tx
    pure (n, ts ++ [ty])
splitPred _ = Nothing

checkPred :: Map Name MonoType -> Pred -> TestGen Bool
checkPred varMap p = case splitPred p of
    Nothing -> pure False
    Just (className, params0) -> do
        let params1 = map (substituteVars varMap) params0
        mclass <- lift $ getClass className
        case mclass of
            Nothing -> pure False
            Just c -> checkClass c params1

zipParamVars :: [Type] -> [MonoType] -> Map Name MonoType
zipParamVars paramVars params =
    mconcat $
        [zipVars v p | (v, p) <- zip paramVars params]

applyParamVars :: Instance -> Map Name MonoType -> [MonoType]
applyParamVars i varMap = map (substituteVars varMap) i.params

checkInstancePreds :: Instance -> [MonoType] -> TestGen Bool
checkInstancePreds i params = do
    let varMap = zipParamVars i.params params
    predMatches <- mapM (checkPred varMap) i.preds
    pure $ and predMatches

-- Avoids the need to zipParamVars if the map of vars has
-- already been generated, e.g. we're checking the preds
-- of the instance that we've generated a candidate for.
checkInstancePreds_ :: Instance -> Map Name MonoType -> TestGen Bool
checkInstancePreds_ i varMap = and
    <$> mapM (checkPred varMap) i.preds

checkClassPreds :: Class -> [MonoType] -> TestGen Bool
checkClassPreds c params = do
    let varMap = M.fromList (zip c.params params)
    predMatches <- mapM (checkPred varMap) c.preds
    pure $ and predMatches

checkClass :: Class -> [MonoType] -> TestGen Bool
checkClass c params = do
    is <- lift $ findInstancesParams c.name params
    instanceMatches <- mapM (\i -> checkInstancePreds i params) is
    predsMatch <- checkClassPreds c params
    pure $ or instanceMatches && predsMatch

-- Check each parameter individually against the env classes
checkEnvClasses :: [MonoType] -> TestGen Bool
checkEnvClasses ts = do
    env <- ask
    -- TODO: This is a great example of where allM from 'extra' is useful...
    let checks = [ checkEnvClass className t | className <- env.candidateInsts, t <- ts]
    and <$> sequence checks
    where
        checkEnvClass :: Name -> MonoType -> TestGen Bool
        checkEnvClass n t = do
            mc <- lift $ getClass n
            case mc of
                Nothing -> pure False
                Just c -> checkClass c [t]

checkCandidate :: Instance -> Map Name MonoType -> TestGen Bool
checkCandidate i varMap = do
    let params = applyParamVars i varMap
    validEnv <- checkEnvClasses params
    validPreds <- checkInstancePreds_ i varMap
    mc <- lift $ getClass i.className
    validClassPreds <- case mc of
        Nothing -> pure False
        Just c -> checkClassPreds c params
    pure $ validEnv && validPreds && validClassPreds

genCandidateVars :: Instance -> TestGen [Map Name MonoType]
genCandidateVars i = do
    env <- ask
    let vars = nub $ concatMap typeVars i.params
    pure $ varPermutations vars env.typeCandidates 

validCandidates :: Instance -> TestGen [[MonoType]]
validCandidates i = do
    candidates <- genCandidateVars i
    valids0 <- filterM (checkCandidate i) candidates
    let valids = map (applyParamVars i) valids0
    env <- ask
    pure $ limitElems env.candidatesLimit valids
