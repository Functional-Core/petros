{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Petros.Text.Parse
    ( Parser (..)
    , runParser
    , runParserEof
    , digitToInt 
    , parseChar
    , parseDigit
    , decimal
    , hexadecimal
    , rational
    , double
    , eof
    , space
    , tab
    , whitespace
    , oneof
    , signed
    , withDefault
    , string
    , consume
    , perhaps
    , newline
    , surrounds
    , parens
    , squares
    , braces
    , quotes
    , squotes
    ) where

-- TODO
-- Provide an example of using the monad
-- and applicative instances.

-- TODO
-- Note about using a proper parsing library.
-- For in depth handling of Unicode, use text-icu.

import Control.Applicative (Alternative (..))
import Data.Char qualified as C
import Data.Text (Text, length, splitAt, uncons)
import Data.Text.Read qualified as TR
    ( Reader
    , decimal
    , double
    , hexadecimal
    , rational
    )
import GHC.Show (Show (show))
import Petros.Internal.Basics
    ( Applicative (..)
    , Char
    , Double
    , Either (..)
    , Eq (..)
    , Float
    , Functor (..)
    , Int
    , Integer
    , Maybe (..)
    , Monad (..)
    , Monoid (..)
    , Num (..)
    , Semigroup ((<>))
    , String
    , otherwise
    , ($)
    )
import Petros.Numeric (Fractional, Integral (..), Natural, UInt)
import Petros.Text (isEmpty)
import Petros.Text.Parse.Result

newtype Parser a = Parser {parse :: Text -> ParseResult a}

runParser :: Parser a -> Text -> ParseResult a
runParser p str = (parse p) str

runParserEof :: Parser a -> Text -> Either ParseError a
runParserEof p0 str = case res of
    Right (x, _) -> Right x
    Left err -> Left err
    where
        res = (parse p1) str
        p1 = p0 <* eof

instance Functor Parser where
    fmap f p = Parser \str -> fmap g $ runParser p str
        where
            g (x, rest) = (f x, rest)

instance Applicative Parser where
    pure x = Parser \str -> success x str
    (<*>) pf pa = do
        f <- pf
        a <- pa
        pure (f a)

-- Maybe style combination semantics
instance Alternative Parser where
    empty = Parser \_ -> Left mempty
    (<|>) px py = Parser \str ->
        case runParser px str of
            rx@(Left _) ->
                case runParser py str of
                    ry@(Left _) -> rx <> ry
                    res -> res
            res -> res

instance Monad Parser where
    (>>=) p f = Parser \str ->
        case runParser p str of
            Right (x, rest) -> runParser (f x) rest
            Left err -> Left err

--------------------------------------------------------------------

readerToParseResult :: Either String (a, Text) -> ParseResult a
readerToParseResult (Left err) = invalidParse err
readerToParseResult (Right (x, rest)) = success x rest

wrapTextReader :: TR.Reader a -> Parser a
wrapTextReader reader = Parser \case
    str
        | isEmpty str -> endOfInput
        | otherwise -> readerToParseResult $ reader str

--------------------------------------------------------------------

digitToInt :: (Integral a) => Char -> Maybe a
digitToInt c
    | C.isHexDigit c = Just (fromInteger $ toInteger $ C.digitToInt c)
    | otherwise = Nothing
{-# INLINE digitToInt #-}
{-# SPECIALIZE digitToInt :: Char -> Maybe Int #-}
{-# SPECIALIZE digitToInt :: Char -> Maybe UInt #-}
{-# SPECIALIZE digitToInt :: Char -> Maybe Integer #-}
{-# SPECIALIZE digitToInt :: Char -> Maybe Natural #-}

parseChar :: (Char -> Text -> ParseResult a) -> Parser a
parseChar f = Parser go
    where
        go str
            | Just (c, rest) <- uncons str = f c rest
            | otherwise = endOfInput

parseDigit :: (Integral a) => Parser a
parseDigit = parseChar \c rest ->
    case digitToInt c of
        Nothing ->
            invalidParse $
                "Character " <> show c <> " is not a hex digit"
        Just n -> success n rest
{-# SPECIALIZE parseDigit :: Parser Int #-}
{-# SPECIALIZE parseDigit :: Parser UInt #-}
{-# SPECIALIZE parseDigit :: Parser Integer #-}
{-# SPECIALIZE parseDigit :: Parser Natural #-}

decimal :: (Integral a) => Parser a
decimal = wrapTextReader TR.decimal
{-# SPECIALIZE decimal :: Parser Int #-}
{-# SPECIALIZE decimal :: Parser UInt #-}
{-# SPECIALIZE decimal :: Parser Integer #-}
{-# SPECIALIZE decimal :: Parser Natural #-}

hexadecimal :: (Integral a) => Parser a
hexadecimal = wrapTextReader TR.hexadecimal
{-# SPECIALIZE hexadecimal :: Parser Int #-}
{-# SPECIALIZE hexadecimal :: Parser UInt #-}
{-# SPECIALIZE hexadecimal :: Parser Integer #-}
{-# SPECIALIZE hexadecimal :: Parser Natural #-}

oneof :: Parser a -> Parser a -> Parser a
oneof = (<|>)
{-# INLINE oneof #-}

withDefault :: a -> Parser a -> Parser a
withDefault def p = Parser \str ->
    let res = runParser p str
     in if isError res then success def str else res

char :: Char -> Parser Char
char expected = parseChar go
    where
        go c rest
            | c == expected = success c rest
            | otherwise =
                invalidParse $
                    "Expected " <> show expected <> " but got " <> show c

signed :: (Num a) => Parser a -> Parser a
signed p = positive <|> negative
    where
        positive = char '+' *> p
        negative = char '-' *> fmap negate p
{-# INLINE signed #-}
{-# SPECIALIZE signed :: Parser Int -> Parser Int #-}
{-# SPECIALIZE signed :: Parser Integer -> Parser Integer #-}

rational :: (Fractional a) => Parser a
rational = wrapTextReader TR.rational
{-# SPECIALIZE rational :: Parser Float #-}
{-# SPECIALIZE rational :: Parser Double #-}

double :: Parser Double
double = wrapTextReader TR.double

string :: Text -> Parser Text
string expected = Parser $ \str ->
    let (actual, rest) = splitAt (length expected) str
     in if actual == expected
            then success actual rest
            else
                invalidParse $
                    "Expected " <> show expected <> " but got " <> show actual

eof :: Parser ()
eof = Parser \case
    str
        | isEmpty str -> success () ""
        | otherwise ->
            invalidParse $
                "Expected end of input, got " <> show str

tab :: Parser Char
tab = char '\t'

space :: Parser Char
space = char ' '

-- Usually used with *> or <*
-- use with `many` or `some`
whitespace :: Parser Char
whitespace = space <|> tab
{-# INLINE whitespace #-}

newline :: Parser Char
newline = char '\n'

surrounds :: (Char, Char) -> Parser a -> Parser a
surrounds (before, after) p = char before *> p <* char after
{-# INLINE surrounds #-}

parens :: Parser a -> Parser a
parens = surrounds ('(', ')')
{-# INLINE parens #-}

squares :: Parser a -> Parser a
squares = surrounds ('[', ']')
{-# INLINE squares #-}

braces :: Parser a -> Parser a
braces = surrounds ('{', '}')
{-# INLINE braces #-}

quotes :: Parser a -> Parser a
quotes = surrounds ('\"', '\"')
{-# INLINE quotes #-}

squotes :: Parser a -> Parser a
squotes = surrounds ('\'', '\'')
{-# INLINE squotes #-}

consume :: Parser Text
consume = Parser \str -> Right (str, "")

perhaps :: Parser a -> Parser (Maybe a)
perhaps p = Parser \str -> case runParser p str of
    Left _ -> success Nothing str
    Right (x, rest) -> success (Just x) rest

-----------------------------------------------------

-- teststr :: Text
-- teststr = "     (10) potato"
--
-- testParser :: Parser (Int, Text)
-- testParser = do
--     _ <- many whitespace
--     n <- parens decimal
--     _ <- space
--     s <- string "onion" <|> string "potato"
--     pure (n, s)
--
-- testParserApp :: Parser (Int, Text)
-- testParserApp =
--     (,)
--         <$> (many whitespace *> parens decimal)
--         <*> (space *> (string "onion" <|> string "potato"))
