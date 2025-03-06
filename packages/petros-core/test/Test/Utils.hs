module Test.Utils where

import Prelude
import Language.Haskell.TH

pprintType :: Type -> String
pprintType (AppT tx ty) = pprintType tx <> " " <> pprintType ty
pprintType (ConT name) = nameBase name
pprintType x = show x

limitElems :: Maybe Int -> [a] -> [a]
limitElems Nothing xs = xs
limitElems (Just limit) xs = take limit xs

pprintTypes :: [Type] -> String
pprintTypes [] = ""
pprintTypes (t:ts) = "(" <> pprintType t <> ") " <> pprintTypes ts

applyTypes :: Exp -> [Type] -> Exp
applyTypes e [] = e
applyTypes e (t:ts) = applyTypes (AppTypeE e t) ts
