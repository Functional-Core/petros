{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Class where

import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Test.Instance
import Test.Law
import Prelude hiding (pred)

data Class = Class
    { name :: Name
    , params :: [Name]
    , preds :: [Pred]
    , laws :: [Law]
    }
    deriving stock (Show)

classLabel :: Class -> String
classLabel c = nameBase c.name

classVars :: [TyVarBndr BndrVis] -> Maybe [Name]
classVars = sequence . map go
    where
        go (PlainTV n _) = Just n
        go (KindedTV n _ StarT) = Just n
        go _ = Nothing

mapClassVars :: Class -> [Type] -> Map Name Type
mapClassVars c ts = M.fromList $ zip c.params ts

type Method = Dec

isLaw :: Name -> Bool
isLaw name = "law_" `isPrefixOf` (nameBase name)

findLaws :: [Method] -> [Law]
findLaws ms = [mkLaw name | SigD name _ <- ms, isLaw name]

getClass :: Name -> Q (Maybe Class)
getClass n = do
    info <- reify n
    case info of
        ClassI (ClassD preds _ params0 _ methods) _ -> do
            let laws = findLaws methods
            pure $ fmap (\params -> Class n params preds laws) (classVars params0)
        _ -> error $ "Could not find class " <> (nameBase n)
