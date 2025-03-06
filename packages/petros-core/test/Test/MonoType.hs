{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.MonoType
    ( MonoType (inner)
    , substituteVars
    , pattern MonoType
    , mapMonoType
    , bimapMonoType
    , isPolymorphic
    , mkMonoType
    ) where

import Language.Haskell.TH
import Data.Map (Map)
import qualified Data.Map as M
import Prelude

newtype MonoType
    = MonoType_ {inner :: Type}
    deriving stock (Show)

pattern MonoType :: Type -> MonoType
pattern MonoType t <- MonoType_ t

isPolymorphic :: Type -> Bool
isPolymorphic (ForallT _ _ _) = True
isPolymorphic (ForallVisT _ _) = True
isPolymorphic (AppT tx ty) =
    isPolymorphic tx || isPolymorphic ty
isPolymorphic (AppKindT _ _) = True
isPolymorphic (SigT _ _) = True
isPolymorphic (VarT _) = True
isPolymorphic (InfixT tx _ ty) =
    isPolymorphic tx || isPolymorphic ty
isPolymorphic (UInfixT tx _ ty) =
    isPolymorphic tx || isPolymorphic ty
isPolymorphic (PromotedInfixT tx _ ty) =
    isPolymorphic tx || isPolymorphic ty
isPolymorphic (PromotedUInfixT tx _ ty) =
    isPolymorphic tx || isPolymorphic ty
isPolymorphic (ParensT t) = isPolymorphic t
isPolymorphic (ImplicitParamT _ t) = isPolymorphic t
isPolymorphic _ = False

mkMonoType :: Type -> Maybe MonoType
mkMonoType t
    | isPolymorphic t = Nothing
    | otherwise = Just (MonoType_ t)

substituteVars :: Map Name MonoType -> Type -> MonoType
substituteVars vars (VarT n) = (vars M.! n)
substituteVars vars (AppT x0 y0) =
    let (MonoType_ x1) = substituteVars vars x0
        (MonoType_ y1) = substituteVars vars y0
     in MonoType_ $ AppT x1 y1
substituteVars vars (ForallT _ _ t) =
    substituteVars vars t
substituteVars vars (ForallVisT _ t) =
    substituteVars vars t
substituteVars vars (AppKindT t0 k0) =
    let (MonoType_ t) = substituteVars vars t0
        (MonoType_ k) = substituteVars vars k0
     in MonoType_ $ AppKindT t k
substituteVars vars (SigT t0 k0) =
    let (MonoType_ t) = substituteVars vars t0
        (MonoType_ k) = substituteVars vars k0
     in MonoType_ $ SigT t k
substituteVars vars (InfixT x0 n y0) =
    let (MonoType_ x) = substituteVars vars x0
        (MonoType_ y) = substituteVars vars y0
     in MonoType_ $ InfixT x n y
substituteVars vars (UInfixT x0 n y0) =
    let (MonoType_ x) = substituteVars vars x0
        (MonoType_ y) = substituteVars vars y0
     in MonoType_ $ UInfixT x n y
substituteVars vars (PromotedInfixT x0 n y0) =
    let (MonoType_ x) = substituteVars vars x0
        (MonoType_ y) = substituteVars vars y0
     in MonoType_ $ PromotedInfixT x n y
substituteVars vars (PromotedUInfixT x0 n y0) =
    let (MonoType_ x) = substituteVars vars x0
        (MonoType_ y) = substituteVars vars y0
     in MonoType_ $ PromotedUInfixT x n y
substituteVars vars (ImplicitParamT s t0) =
    let (MonoType_ t) = substituteVars vars t0
     in MonoType_ $ ImplicitParamT s t
substituteVars _ t = MonoType_ t

mapMonoType :: Semigroup b => (MonoType -> b) -> MonoType -> b
mapMonoType f (MonoType_ (AppT tx ty)) = f (MonoType_ tx) <> f (MonoType_ ty)
mapMonoType f t = f t

bimapMonoType :: Semigroup b => (MonoType -> b) -> (MonoType -> b) -> MonoType -> b
bimapMonoType f g (MonoType_ (AppT tx ty)) = f (MonoType_ tx) <> g (MonoType_ ty)
bimapMonoType f _ t = f t
