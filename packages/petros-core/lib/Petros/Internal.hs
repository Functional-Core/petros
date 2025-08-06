{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Internal
    ( FromPrelude (..)
    , liftPrelude
    , liftPrelude2
    ) where

import GHC.Generics
import Data.Coerce ( coerce )

newtype FromPrelude a = FromPrelude a
    deriving stock (Generic)

liftPrelude :: (a -> b) -> FromPrelude a -> b
liftPrelude op x = op (coerce x)
{-# INLINE liftPrelude #-}

liftPrelude2 :: (a -> a -> b) -> FromPrelude a -> FromPrelude a -> b
liftPrelude2 op x y = coerce x `op` coerce y
{-# INLINE liftPrelude2 #-}
