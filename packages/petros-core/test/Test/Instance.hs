{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Instance where

import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Prelude hiding (pred)
import Test.MonoType

data Instance = Instance
    { className :: Name
    , preds :: [Pred]
    , params :: [Type]
    }
    deriving stock (Show)

instanceParams :: Type -> Maybe [Type]
instanceParams (ConT _className) = Just []
instanceParams (AppT tx ty) = do
    ts <- instanceParams tx
    pure (ts ++ [ty])
instanceParams _ = Nothing

mkInstance :: Name -> Dec -> Maybe Instance
mkInstance n (InstanceD _ preds t _) =
    Instance n preds <$> instanceParams t
mkInstance _ _ = Nothing

findInstances :: Name -> Q [Instance]
findInstances className = do
    ClassI _ is <- reify className
    pure $ mapMaybe (mkInstance className) is

findInstancesParams :: Name -> [MonoType] -> Q [Instance]
findInstancesParams className params = do
    is <- reifyInstances className (map inner params)
    pure $ mapMaybe (mkInstance className) is

