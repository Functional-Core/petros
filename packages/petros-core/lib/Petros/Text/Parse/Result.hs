{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

module Petros.Text.Parse.Result
    ( ParseError ()
    , ParseResult
    , isSuccess
    , isError
    , endOfInput
    , invalidParse
    , success
    ) where

import Data.Text (Text)
import Petros.Internal.Basics

data ParseError
    = EndOfInput
    | InvalidParse String
    | Many [ParseError]
    deriving stock (Show, Eq)

instance Semigroup ParseError where
    (<>) (Many xs) (Many ys) = Many (xs <> ys)
    (<>) (Many xs) err = Many (xs <> [err])
    (<>) err (Many ys) = Many (err : ys)
    (<>) ex ey = Many [ex, ey]

instance Monoid ParseError where
    mempty = Many []

-- (a, rest)
type ParseResult a = Either ParseError (a, Text)

isSuccess :: ParseResult a -> Bool
isSuccess (Right _) = True
isSuccess _ = False
{-# INLINE isSuccess #-}

isError :: ParseResult a -> Bool
isError (Left _) = True
isError _ = True
{-# INLINE isError #-}

endOfInput :: ParseResult a
endOfInput = Left EndOfInput

invalidParse :: String -> ParseResult a
invalidParse msg = Left $ InvalidParse msg

success :: a -> Text -> ParseResult a
success x rest = Right (x, rest)
