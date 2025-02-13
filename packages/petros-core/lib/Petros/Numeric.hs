{-# LANGUAGE Trustworthy #-}

{- |
Module : Petros.Numeric
Copyright : 2025 Functional Core
SPDX-License-Identifier : MPL-2.0
Maintainer : James Burton <james@functionalcore.dev>
Stability : Stable

Provides types and functions for working
with numerical data.
-}
module Petros.Numeric
    ( module Data.Int
    , module GHC.Base
    , module GHC.Float
    , module GHC.Num
    , UInt
    , UInt8
    , UInt16
    , UInt32
    , UInt64
    , module Data.Bits
    ) where

import Data.Bits (Bits (..), FiniteBits (..), (.<<.), (.>>.), (.^.))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Base (maxInt, minInt)
import GHC.Float (Double (..), Float (..), Floating (..), RealFloat (..))
import GHC.Num (Integer (..), Natural (..), Num (..))

type UInt = Word
type UInt8 = Word8
type UInt16 = Word16
type UInt32 = Word32
type UInt64 = Word64

