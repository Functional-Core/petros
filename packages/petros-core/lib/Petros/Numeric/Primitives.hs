{-# LANGUAGE Trustworthy #-}

module Petros.Numeric.Primitives
    ( module Data.Int
    , module Data.Ratio
    , module GHC.Base
    , module GHC.Float
    , module GHC.Num
    , module GHC.Real
    , UInt
    , UInt8
    , UInt16
    , UInt32
    , UInt64
    , module Data.Bits
    , isEven
    , isOdd
    ) where

import Data.Bits (Bits (..), FiniteBits (..), (.<<.), (.>>.), (.^.))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ratio (approxRational)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Base (maxInt, minInt)
import GHC.Float (Double (..), Float (..), Floating (..), RealFloat (..))
import GHC.Num (Integer (..), Natural (..), Num (..))
import GHC.Real
    ( Fractional (..)
    , Integral (..)
    , Ratio (..)
    , Rational
    , Real (..)
    , RealFrac (..)
    , denominator
    , fromIntegral
    , gcd
    , infinity
    , integralEnumFrom
    , integralEnumFromThen
    , integralEnumFromThenTo
    , integralEnumFromTo
    , lcm
    , notANumber
    , numerator
    , numericEnumFrom
    , numericEnumFromThen
    , numericEnumFromThenTo
    , numericEnumFromTo
    , reduce
    , (%)
    , (^)
    , (^^)
    )

import GHC.Real qualified as GHC (even, odd)
import Data.Bool (Bool (..))

type UInt = Word
type UInt8 = Word8
type UInt16 = Word16
type UInt32 = Word32
type UInt64 = Word64

isEven :: (Integral a) => a -> Bool
isEven = GHC.even
{-# INLINE isEven #-}

isOdd :: (Integral a) => a -> Bool
isOdd = GHC.odd
{-# INLINE isOdd #-}
