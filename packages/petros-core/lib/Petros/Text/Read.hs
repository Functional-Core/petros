{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}

module Petros.Text.Read
    ( module Text.Read
    , unsafeRead
    , ReadError (..)
    , ReadResult
    , readMaybe
    , readEither
    ) where

import Text.Read (Read (..))

import Data.Text (Text, unpack)
import Petros.Internal.Basics
import Text.Read qualified as R

data ReadError = NoParse | AmbiguousParse | UnknownError String
    deriving stock (Show, Read, Eq)

type ReadResult a = Either ReadError a

toReadError :: String -> ReadError
toReadError "Prelude.read: no parse" = NoParse
toReadError "Prelude.read: ambiguous parse" = AmbiguousParse
toReadError err = UnknownError err

toReadResult :: Either String a -> ReadResult a
toReadResult (Left err) = Left $ toReadError err
toReadResult (Right x) = Right x

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = R.readMaybe . unpack
{-# INLINE readMaybe #-}

readEither :: (Read a) => Text -> ReadResult a
readEither = toReadResult . R.readEither . unpack
{-# INLINE readEither #-}

unsafeRead :: (Read a) => Text -> a
unsafeRead = R.read . unpack
{-# INLINE unsafeRead #-}
