{-# LANGUAGE Safe #-}

-- TODO: mention text-icu package
module Petros.Text.Encoding
    ( module Data.Text.Encoding
    , decodeUtf8
    , decodeUtf8_
    ) where

import Data.Text.Encoding (encodeUtf8, encodeUtf8Builder)

import Data.Text (Text)
import Data.Text.Encoding qualified as T (decodeUtf8Lenient, decodeUtf8')
import Data.ByteString (ByteString)
import Petros.Internal.Basics
import Data.Text.Encoding.Error (UnicodeException)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8Lenient
{-# INLINE decodeUtf8 #-}

decodeUtf8_ :: ByteString -> Either UnicodeException Text
decodeUtf8_ = T.decodeUtf8'
{-# INLINE decodeUtf8_ #-}
