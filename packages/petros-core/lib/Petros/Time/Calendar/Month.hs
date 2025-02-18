{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

module Petros.Time.Calendar.Month
    ( MonthOfYear
        ( ..
        , January
        , February
        , March
        , April
        , May
        , June
        , July
        , August
        , September
        , October
        , November
        , December
        )
    , addMonths
    , addMonths_
    , module Data.Time.Calendar.Month
    ) where

import Data.Time.Calendar.Month
    ( Month (..)
    , diffMonths
    )

import Data.Time.Calendar.Month qualified as T (addMonths)
import Petros.Numeric (Natural, UInt, UInt8)
import Prelude

addMonths :: (Integral n) => n -> Month -> Month
addMonths n m = T.addMonths (fromIntegral n) m
{-# SPECIALIZE addMonths :: Integer -> Month -> Month #-}
{-# SPECIALIZE addMonths :: Int -> Month -> Month #-}
{-# SPECIALIZE addMonths :: UInt -> Month -> Month #-}
{-# SPECIALIZE addMonths :: Natural -> Month -> Month #-}

addMonths_ :: Integer -> Month -> Month
addMonths_ = T.addMonths
{-# INLINE addMonths_ #-}

------------------------------------------------

data MonthOfYear = MonthOfYear UInt8
    deriving stock (Show, Eq)

pattern January :: MonthOfYear
pattern January = MonthOfYear 1

pattern February :: MonthOfYear
pattern February = MonthOfYear 2

pattern March :: MonthOfYear
pattern March = MonthOfYear 3

pattern April :: MonthOfYear
pattern April = MonthOfYear 4

pattern May :: MonthOfYear
pattern May = MonthOfYear 5

pattern June :: MonthOfYear
pattern June = MonthOfYear 6

pattern July :: MonthOfYear
pattern July = MonthOfYear 7

pattern August :: MonthOfYear
pattern August = MonthOfYear 8

pattern September :: MonthOfYear
pattern September = MonthOfYear 9

pattern October :: MonthOfYear
pattern October = MonthOfYear 10

pattern November :: MonthOfYear
pattern November = MonthOfYear 11

pattern December :: MonthOfYear
pattern December = MonthOfYear 12

{-# COMPLETE January, February, March, April, May, June, July, August, September, October, November, December #-}
