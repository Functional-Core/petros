{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Law where

import Language.Haskell.TH
import Prelude

data Law = Law
    { name :: Name
    , label :: String
    }
    deriving stock (Show)

lawLabel :: Name -> String
lawLabel = drop 4 . nameBase

mkLaw :: Name -> Law
mkLaw n = Law n (lawLabel n)
