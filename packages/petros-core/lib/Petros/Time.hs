{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}

-- GOALS:
-- Parse and output dates/times in arbitrary formats
--
module Petros.Time
    ( module Data.Time
    , ISO8601
    , iso8601Show
    , iso8601Parse
    ) where

-- TODO:
-- Include functions for parsing/showing
-- arbitrary formats for use when converting
-- i.e. in the Tabor App.

import Data.Time ()

import Data.Time.Format.ISO8601 (ISO8601)

-- TODO:
-- Delete the individual modules
-- Just re-export Calendar, Clock, Local and Format
-- Change Strings to Text etc as usual
-- Keep it basic, we can add extra stuff later

import Data.String (IsString (..))
import Data.Text (Text, unpack)
import Data.Time.Format.ISO8601 qualified as F
import GHC.Real (Integral (toInteger))
import Petros.Binary (ByteString, LByteString, ShortByteString)
import Petros.Internal.Basics
import Petros.Numeric (Natural, UInt, UInt8)
import Petros.Text (LText)

---------------------------------------------------------
-- Calendar
---------------------------------------------------------
---------------------------------------------------------
-- Clock
---------------------------------------------------------
---------------------------------------------------------
-- Local
---------------------------------------------------------
---------------------------------------------------------
-- Format
---------------------------------------------------------
iso8601Show :: (IsString s, ISO8601 t) => t -> s
iso8601Show = fromString . F.iso8601Show
{-# INLINE iso8601Show #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> Text #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> LText #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> ByteString #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> LByteString #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> ShortByteString #-}
{-# SPECIALIZE iso8601Show :: (ISO8601 t) => t -> String #-}

iso8601Parse :: (ISO8601 t) => Text -> Maybe t
iso8601Parse = F.iso8601ParseM . unpack
{-# INLINE iso8601Parse #-}
