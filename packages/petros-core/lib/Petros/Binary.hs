{-# LANGUAGE Safe #-}

-- TODO: Should this be called Petros.Bytes?
-- ShortBS for if you need lots of small bytestrings
-- kept for a long time to avoid heap fragmentation.
-- Can also just run `copy` on a normal BS.
module Petros.Binary
    ( module Data.ByteString
    , module Data.ByteString.Short
    , LByteString
    , isEmpty
    , maybeHead
    , maybeTail
    , maybeInit
    , maybeLast
    , maybeMaximum
    , maybeMinimum
    , unsafeHead
    , unsafeInit
    , unsafeTail
    , unsafeLast
    , unsafeFoldl1
    , unsafeFoldl1'
    , unsafeFoldr1
    , unsafeFoldr1'
    , unsafeMinimum
    , unsafeMaximum
    ) where

import Data.ByteString
    ( ByteString
    , all
    , any
    , append
    , break
    , breakEnd
    , breakSubstring
    , concat
    , concatMap
    , cons
    , copy
    , count
    , drop
    , dropEnd
    , dropWhile
    , dropWhileEnd
    , elem
    , elemIndex
    , elemIndexEnd
    , empty
    , filter
    , find
    , findIndex
    , findIndexEnd
    , findIndices
    , foldl
    , foldl'
    , foldr
    , foldr'
    , fromFilePath
    , fromStrict
    , group
    , groupBy
    , index
    , indexMaybe
    , inits
    , initsNE
    , intercalate
    , intersperse
    , isInfixOf
    , isPrefixOf
    , isSuffixOf
    , isValidUtf8
    , length
    , map
    , mapAccumL
    , mapAccumR
    , notElem
    , pack
    , packZipWith
    , partition
    , replicate
    , reverse
    , scanl
    , scanl1
    , scanr
    , scanr1
    , singleton
    , snoc
    , sort
    , span
    , spanEnd
    , split
    , splitAt
    , splitWith
    , stripPrefix
    , stripSuffix
    , tails
    , tailsNE
    , take
    , takeEnd
    , takeWhile
    , takeWhileEnd
    , toFilePath
    , toStrict
    , transpose
    , uncons
    , unfoldr
    , unfoldrN
    , unpack
    , unsnoc
    , unzip
    , zip
    , zipWith
    , (!?)
    )
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB (ByteString)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)

import Petros.Internal.Basics
import Petros.Numeric (UInt8)

type LByteString = LB.ByteString

isEmpty :: ByteString -> Bool
isEmpty = B.null
{-# INLINE isEmpty #-}

maybeHead :: ByteString -> Maybe UInt8
maybeHead = fmap fst . uncons
{-# INLINE maybeHead #-}

unsafeHead :: ByteString -> UInt8
unsafeHead = B.head
{-# INLINE unsafeHead #-}

maybeTail :: ByteString -> Maybe ByteString
maybeTail = fmap snd . uncons
{-# INLINE maybeTail #-}

unsafeTail :: ByteString -> ByteString
unsafeTail = B.tail
{-# INLINE unsafeTail #-}

maybeInit :: ByteString -> Maybe ByteString
maybeInit = fmap fst . unsnoc
{-# INLINE maybeInit #-}

unsafeInit :: ByteString -> ByteString
unsafeInit = B.init
{-# INLINE unsafeInit #-}

maybeLast :: ByteString -> Maybe UInt8
maybeLast = fmap snd . unsnoc
{-# INLINE maybeLast #-}

unsafeLast :: ByteString -> UInt8
unsafeLast = B.last
{-# INLINE unsafeLast #-}

unsafeFoldl1 :: (UInt8 -> UInt8 -> UInt8) -> ByteString -> UInt8
unsafeFoldl1 = B.foldl1
{-# INLINE unsafeFoldl1 #-}

unsafeFoldl1' :: (UInt8 -> UInt8 -> UInt8) -> ByteString -> UInt8
unsafeFoldl1' = B.foldl1'
{-# INLINE unsafeFoldl1' #-}

unsafeFoldr1 :: (UInt8 -> UInt8 -> UInt8) -> ByteString -> UInt8
unsafeFoldr1 = B.foldr1
{-# INLINE unsafeFoldr1 #-}

unsafeFoldr1' :: (UInt8 -> UInt8 -> UInt8) -> ByteString -> UInt8
unsafeFoldr1' = B.foldr1'
{-# INLINE unsafeFoldr1' #-}

unsafeMaximum :: ByteString -> UInt8
unsafeMaximum = B.maximum
{-# INLINE unsafeMaximum #-}

maybeMaximum :: ByteString -> Maybe UInt8
maybeMaximum bs
    | isEmpty bs = Nothing
    | otherwise = Just (B.maximum bs)
{-# INLINE maybeMaximum #-}

unsafeMinimum :: ByteString -> UInt8
unsafeMinimum = B.minimum
{-# INLINE unsafeMinimum #-}

maybeMinimum :: ByteString -> Maybe UInt8
maybeMinimum bs
    | isEmpty bs = Nothing
    | otherwise = Just (B.minimum bs)
{-# INLINE maybeMinimum #-}
