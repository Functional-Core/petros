{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

module Petros.Internal
    ( FromPrelude (..)
    , liftPrelude
    , liftPrelude2
    ) where

import GHC.Generics
import Prelude

newtype FromPrelude a = FromPrelude a
    deriving stock (Generic)

liftPrelude :: (a -> b) -> FromPrelude a -> b
liftPrelude op (FromPrelude x) = op x
{-# INLINE liftPrelude #-}

liftPrelude2 :: (a -> a -> b) -> FromPrelude a -> FromPrelude a -> b
liftPrelude2 op (FromPrelude x) (FromPrelude y) = x `op` y
{-# INLINE liftPrelude2 #-}
