{-# LANGUAGE Safe #-}

module Petros.Text.Show
    ( module GHC.Show
    , show_
    ) where

import GHC.Show (Show (..))
import GHC.Show qualified as Show
import Petros.Binary
    ( ByteString
    , LByteString
    , ShortByteString
    )
import Petros.Internal.Basics (String, (.))
import Petros.Text (LText, Text)
import Petros.Text.String (IsString (..))

show_ :: (Show a, IsString b) => a -> b
show_ = fromString . Show.show
{-# INLINE show_ #-}
{-# SPECIALIZE show_ :: (Show a) => a -> Text #-}
{-# SPECIALIZE show_ :: (Show a) => a -> LText #-}
{-# SPECIALIZE show_ :: (Show a) => a -> ByteString #-}
{-# SPECIALIZE show_ :: (Show a) => a -> LByteString #-}
{-# SPECIALIZE show_ :: (Show a) => a -> ShortByteString #-}
{-# SPECIALIZE show_ :: (Show a) => a -> String #-}
