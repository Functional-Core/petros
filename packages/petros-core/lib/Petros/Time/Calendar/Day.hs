{-# LANGUAGE Safe #-}

module Petros.Time.Calendar.Day
    ( module Data.Time.Calendar
    , addDays
    , addDays_
    , scaleCalendarDiffDays
    , scaleCalendarDiffDays_
    , (.*)
    , (*.)
    ) where

import Data.Time.Calendar
    ( CalendarDiffDays (..)
    , Day (..)
    , calendarDay
    , calendarMonth
    , calendarWeek
    , calendarYear
    , diffDays
    , showGregorian
    )

import Data.Time.Calendar qualified as T
import Petros.Numeric (Natural, UInt)
import Prelude

addDays :: (Integral n) => n -> Day -> Day
addDays n d = T.addDays (toInteger n) d
{-# SPECIALIZE addDays :: Integer -> Day -> Day #-}
{-# SPECIALIZE addDays :: Int -> Day -> Day #-}
{-# SPECIALIZE addDays :: Word -> Day -> Day #-}
{-# SPECIALIZE addDays :: Natural -> Day -> Day #-}

addDays_ :: Integer -> Day -> Day
addDays_ = T.addDays

scaleCalendarDiffDays :: (Integral n) => n -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays n x = T.scaleCalendarDiffDays (toInteger n) x
{-# SPECIALIZE scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE scaleCalendarDiffDays :: Int -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE scaleCalendarDiffDays :: UInt -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE scaleCalendarDiffDays :: Natural -> CalendarDiffDays -> CalendarDiffDays #-}

scaleCalendarDiffDays_ :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays_ = T.scaleCalendarDiffDays

(.*) :: (Integral n) => n -> CalendarDiffDays -> CalendarDiffDays
(.*) n x = T.scaleCalendarDiffDays (toInteger n) x
{-# SPECIALIZE (.*) :: Integer -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE (.*) :: Int -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE (.*) :: UInt -> CalendarDiffDays -> CalendarDiffDays #-}
{-# SPECIALIZE (.*) :: Natural -> CalendarDiffDays -> CalendarDiffDays #-}

(*.) :: (Integral n) => CalendarDiffDays -> n -> CalendarDiffDays
(*.) = flip (.*)
{-# INLINE (*.) #-}
{-# SPECIALIZE (*.) :: CalendarDiffDays -> Integer -> CalendarDiffDays #-}
{-# SPECIALIZE (*.) :: CalendarDiffDays -> Int -> CalendarDiffDays #-}
{-# SPECIALIZE (*.) :: CalendarDiffDays -> UInt -> CalendarDiffDays #-}
{-# SPECIALIZE (*.) :: CalendarDiffDays -> Natural -> CalendarDiffDays #-}
