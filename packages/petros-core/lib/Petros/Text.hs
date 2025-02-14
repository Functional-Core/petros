{-# LANGUAGE Safe #-}

{- |
Module : Petros.Text
Copyright : 2025 Functional Core
SPDX-License-Identifier : MPL-2.0
Maintainer : James Burton <james@functionalcore.dev>
Stability : Stable

Provides types and functions for working
with textual data.

We follow the guidance found in
https://tech.fpcomplete.com/haskell/tutorial/string-types/
and
https://www.snoyman.com/blog/2016/12/beware-of-readfile/
-}
module Petros.Text
    ( module Data.String
    , module Data.Text
    , module Data.Text.Lazy
    , LText
    , isEmpty
    , index
    , maybeHead
    , maybeTail
    , maybeInit
    , maybeLast
    , unsafeHead
    , unsafeTail
    , unsafeInit
    , unsafeLast
    , unsafeReplace
    , unsafeFoldl1
    , unsafeFoldl1'
    , unsafeFoldr1
    , unsafeMaximum
    , unsafeMinimum
    , unsafeBreakOn
    , unsafeBreakOnEnd
    , unsafeBreakOnAll
    , unsafeSplitOn
    , unsafeIndex
    , unsafeCount
    ) where

import Data.String
    ( IsString (..)
    , String
    )
import Data.Text
    ( Text
    , all
    , any
    , append
    , break
    , center
    , commonPrefixes
    , compareLength
    , concat
    , concatMap
    , cons
    , copy
    , drop
    , dropAround
    , dropEnd
    , dropWhile
    , dropWhileEnd
    , elem
    , empty
    , filter
    , find
    , findIndex
    , foldl
    , foldl'
    , foldr
    , foldr'
    , group
    , groupBy
    , inits
    , intercalate
    , intersperse
    , isAscii
    , isInfixOf
    , isPrefixOf
    , isSuffixOf
    , justifyLeft
    , justifyRight
    , length
    , lines
    , map
    , mapAccumL
    , mapAccumR
    , measureOff
    , pack
    , partition
    , replicate
    , reverse
    , scanl
    , scanl1
    , scanr
    , scanr1
    , singleton
    , snoc
    , span
    , spanEndM
    , spanM
    , split
    , splitAt
    , strip
    , stripEnd
    , stripPrefix
    , stripStart
    , stripSuffix
    , tails
    , take
    , takeEnd
    , takeWhile
    , takeWhileEnd
    , toCaseFold
    , toLower
    , toTitle
    , toUpper
    , transpose
    , uncons
    , unfoldr
    , unfoldrN
    , unlines
    , unpack
    , unsnoc
    , unwords
    , words
    , zip
    , zipWith
    )
import Data.Text.Lazy
    ( foldlChunks
    , foldrChunks
    , fromChunks
    , fromStrict
    , toChunks
    , toStrict
    )

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Petros.Internal.Basics 

type LText = LT.Text

isEmpty :: Text -> Bool
isEmpty = T.null
{-# INLINE isEmpty #-}

unsafeHead :: Text -> Char
unsafeHead = T.head
{-# INLINE unsafeHead #-}

maybeHead :: Text -> Maybe Char
maybeHead = fmap fst . uncons
{-# INLINE maybeHead #-}

unsafeTail :: Text -> Text
unsafeTail = T.tail
{-# INLINE unsafeTail #-}
 
maybeTail :: Text -> Maybe Text
maybeTail = fmap snd . uncons
{-# INLINE maybeTail #-}

unsafeInit :: Text -> Text
unsafeInit = T.init
{-# INLINE unsafeInit #-}

maybeInit :: Text -> Maybe Text
maybeInit = fmap fst . unsnoc
{-# INLINE maybeInit #-}

unsafeLast :: Text -> Char
unsafeLast = T.last
{-# INLINE unsafeLast #-}

maybeLast :: Text -> Maybe Char
maybeLast = fmap snd . unsnoc
{-# INLINE maybeLast #-}

unsafeReplace :: Text -> Text -> Text -> Text
unsafeReplace = T.replace
{-# INLINE unsafeReplace #-}

unsafeFoldl1 :: (Char -> Char -> Char) -> Text -> Char
unsafeFoldl1 = T.foldl1
{-# INLINE unsafeFoldl1 #-}

unsafeFoldl1' :: (Char -> Char -> Char) -> Text -> Char
unsafeFoldl1' = T.foldl1'
{-# INLINE unsafeFoldl1' #-}

unsafeFoldr1 :: (Char -> Char -> Char) -> Text -> Char
unsafeFoldr1 = T.foldr1
{-# INLINE unsafeFoldr1 #-}

unsafeMaximum :: Text -> Char
unsafeMaximum = T.maximum
{-# INLINE unsafeMaximum #-}

unsafeMinimum :: Text -> Char
unsafeMinimum = T.minimum
{-# INLINE unsafeMinimum #-}

unsafeBreakOn :: Text -> Text -> (Text, Text)
unsafeBreakOn = T.breakOn
{-# INLINE unsafeBreakOn #-}

unsafeBreakOnEnd :: Text -> Text -> (Text, Text)
unsafeBreakOnEnd = T.breakOnEnd
{-# INLINE unsafeBreakOnEnd #-}

unsafeBreakOnAll :: Text -> Text -> [(Text, Text)]
unsafeBreakOnAll = T.breakOnAll
{-# INLINE unsafeBreakOnAll #-}

unsafeSplitOn :: Text -> Text -> [Text]
unsafeSplitOn = T.splitOn
{-# INLINE unsafeSplitOn #-}

index :: Text -> Int -> Maybe Char
index str n
    | n < 0 || n >= length str = Nothing
    | otherwise = Just $ unsafeIndex str n

unsafeIndex :: Text -> Int -> Char
unsafeIndex = T.index
{-# INLINE unsafeIndex #-}

unsafeCount :: Text -> Text -> Int
unsafeCount = T.count
{-# INLINE unsafeCount #-}
