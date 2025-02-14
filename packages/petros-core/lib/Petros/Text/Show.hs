{-# LANGUAGE Safe #-}

module Petros.Text.Show
    ( Show
    , show
    ) where

import GHC.Show (Show ())
import GHC.Show qualified as Show
import Petros.Binary
    ( ByteString
    , LByteString
    , ShortByteString
    )
import Petros.Internal.Basics (String, (.))
import Petros.Text (LText, Text)
import Petros.Text.String (IsString (..))

show :: (Show a, IsString b) => a -> b
show = fromString . Show.show
{-# INLINE show #-}
{-# SPECIALIZE show :: (Show a) => a -> Text #-}
{-# SPECIALIZE show :: (Show a) => a -> LText #-}
{-# SPECIALIZE show :: (Show a) => a -> ByteString #-}
{-# SPECIALIZE show :: (Show a) => a -> LByteString #-}
{-# SPECIALIZE show :: (Show a) => a -> ShortByteString #-}
{-# SPECIALIZE show :: (Show a) => a -> String #-}
