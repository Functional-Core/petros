{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Eq.PartialEq
    ( PartialEq (..)
    ) where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Product (..), Sum (..))
import GHC.Generics
import Petros.Internal
import Prelude hiding (Eq (..))
import Prelude qualified (Eq (..))

class PartialEq a where
    (~=) :: a -> a -> Bool
    default (~=) :: (Generic a, GPartialEq (Rep a)) => a -> a -> Bool
    (~=) x y = geqPartial (from x) (from y)
    {-# INLINE (~=) #-}

    (==?) :: a -> a -> Maybe Bool
    default (==?) :: (Generic a, GPartialEq (Rep a)) => a -> a -> Maybe Bool
    (==?) x y = geqMaybe (from x) (from y)
    {-# INLINE (==?) #-}

    (/~=) :: a -> a -> Bool
    (/~=) x y = not (x ~= y)
    {-# INLINE (/~=) #-}

class GPartialEq f where
    geqPartial :: (f a) -> (f a) -> Bool
    geqMaybe :: (f a) -> (f a) -> Maybe Bool

instance GPartialEq V1 where
    geqPartial = undefined
    geqMaybe = undefined

instance GPartialEq U1 where
    geqPartial _ _ = True
    geqMaybe _ _ = Just True

instance (GPartialEq f, GPartialEq g) => GPartialEq (f :+: g) where
    geqPartial (L1 x) (L1 y) = geqPartial x y
    geqPartial (R1 x) (R1 y) = geqPartial x y
    geqPartial _ _ = False

    geqMaybe (L1 x) (L1 y) = geqMaybe x y
    geqMaybe (R1 x) (R1 y) = geqMaybe x y
    geqMaybe _ _ = Just False

instance (GPartialEq f, GPartialEq g) => GPartialEq (f :*: g) where
    geqPartial (x1 :*: y1) (x2 :*: y2) = (geqPartial x1 x2) && (geqPartial y1 y2)
    geqMaybe (x1 :*: y1) (x2 :*: y2) = liftA2 (&&) (geqMaybe x1 x2) (geqMaybe y1 y2)

instance (PartialEq c) => GPartialEq (K1 i c) where
    geqPartial (K1 x) (K1 y) = x ~= y
    geqMaybe (K1 x) (K1 y) = x ==? y

instance (GPartialEq f) => GPartialEq (M1 i t f) where
    geqPartial (M1 x) (M1 y) = geqPartial x y
    geqMaybe (M1 x) (M1 y) = geqMaybe x y

--------------------------------------------------------------------------

instance (Prelude.Eq a) => PartialEq (FromPrelude a) where
    (~=) = liftPrelude2 (Prelude.==)
    (==?) x y = Just $ liftPrelude2 (Prelude.==) x y

deriving via (FromPrelude Ordering) instance PartialEq Ordering

-- deriving via (PreludeEq ByteArray) instance PartialEq ByteArray
-- deriving via (PreludeEq Timeout) instance PartialEq Timeout
-- deriving via (PreludeEq BigNat) instance PartialEq BigNat
-- deriving via (PreludeEq Void) instance PartialEq Void
-- deriving via (PreludeEq ByteOrder) instance PartialEq ByteOrder
-- deriving via (PreludeEq ClosureType) instance PartialEq ClosureType
-- deriving via (PreludeEq BlockReason) instance PartialEq BlockReason
-- deriving via (PreludeEq ThreadId) instance PartialEq ThreadId
-- deriving via (PreludeEq ThreadStatus) instance PartialEq ThreadStatus
-- deriving via (PreludeEq Constr) instance PartialEq Constr
-- deriving via (PreludeEq ConstrRep) instance PartialEq ConstrRep
-- deriving via (PreludeEq DataRep) instance PartialEq DataRep
-- deriving via (PreludeEq Fixity) instance PartialEq Fixity
-- deriving via (PreludeEq All) instance PartialEq All
-- deriving via (PreludeEq Any) instance PartialEq Any
-- deriving via (PreludeEq SomeTypeRep) instance PartialEq SomeTypeRep
-- deriving via (PreludeEq Unique) instance PartialEq Unique
-- deriving via (PreludeEq Version) instance PartialEq Version
-- deriving via (PreludeEq ControlMessage) instance PartialEq ControlMessage
-- deriving via (PreludeEq Event) instance PartialEq Event
-- deriving via (PreludeEq EventLifetime) instance PartialEq EventLifetime
-- deriving via (PreludeEq Lifetime) instance PartialEq Lifetime
-- deriving via (PreludeEq FdKey) instance PartialEq FdKey
-- deriving via (PreludeEq State) instance PartialEq State
-- deriving via (PreludeEq TimeoutKey) instance PartialEq TimeoutKey
-- deriving via (PreludeEq ErrorCall) instance PartialEq ErrorCall
-- deriving via (PreludeEq ArithException) instance PartialEq ArithException
-- deriving via (PreludeEq SpecConstrAnnotation) instance PartialEq SpecConstrAnnotation
-- deriving via (PreludeEq Fingerprint) instance PartialEq Fingerprint
-- deriving via (PreludeEq Errno) instance PartialEq Errno
-- deriving via (PreludeEq CBool) instance PartialEq CBool
-- deriving via (PreludeEq CChar) instance PartialEq CChar
-- deriving via (PreludeEq CClock) instance PartialEq CClock
-- deriving via (PreludeEq CDouble) instance PartialEq CDouble
-- deriving via (PreludeEq CFloat) instance PartialEq CFloat
-- deriving via (PreludeEq CInt) instance PartialEq CInt
-- deriving via (PreludeEq CIntMax) instance PartialEq CIntMax
-- deriving via (PreludeEq CIntPtr) instance PartialEq CIntPtr
-- deriving via (PreludeEq CLLong) instance PartialEq CLLong
-- deriving via (PreludeEq CLong) instance PartialEq CLong
-- deriving via (PreludeEq CPtrdiff) instance PartialEq CPtrdiff
-- deriving via (PreludeEq CSChar) instance PartialEq CSChar
-- deriving via (PreludeEq CSUSeconds) instance PartialEq CSUSeconds
-- deriving via (PreludeEq CShort) instance PartialEq CShort
-- deriving via (PreludeEq CSigAtomic) instance PartialEq CSigAtomic
-- deriving via (PreludeEq CSize) instance PartialEq CSize
-- deriving via (PreludeEq CTime) instance PartialEq CTime
-- deriving via (PreludeEq CUChar) instance PartialEq CUChar
-- deriving via (PreludeEq CUInt) instance PartialEq CUInt
-- deriving via (PreludeEq CUIntMax) instance PartialEq CUIntMax
-- deriving via (PreludeEq CUIntPtr) instance PartialEq CUIntPtr
-- deriving via (PreludeEq CULLong) instance PartialEq CULLong
-- deriving via (PreludeEq CULong) instance PartialEq CULong
-- deriving via (PreludeEq CUSeconds) instance PartialEq CUSeconds
-- deriving via (PreludeEq CUShort) instance PartialEq CUShort
-- deriving via (PreludeEq CWchar) instance PartialEq CWchar
-- deriving via (PreludeEq IntPtr) instance PartialEq IntPtr
-- deriving via (PreludeEq WordPtr) instance PartialEq WordPtr
-- deriving via (PreludeEq ForeignSrcLang) instance PartialEq ForeignSrcLang
-- deriving via (PreludeEq Associativity) instance PartialEq Associativity
-- deriving via (PreludeEq DecidedStrictness) instance PartialEq DecidedStrictness
-- deriving via (PreludeEq Fixity) instance PartialEq Fixity
-- deriving via (PreludeEq SourceStrictness) instance PartialEq SourceStrictness
-- deriving via (PreludeEq SourceUnpackedness) instance PartialEq SourceUnpackedness
-- deriving via (PreludeEq MaskingState) instance PartialEq MaskingState
-- deriving via (PreludeEq BufferState) instance PartialEq BufferState
-- deriving via (PreludeEq IODeviceType) instance PartialEq IODeviceType
-- deriving via (PreludeEq SeekMode) instance PartialEq SeekMode
-- deriving via (PreludeEq CodingProgress) instance PartialEq CodingProgress
-- deriving via (PreludeEq ArrayException) instance PartialEq ArrayException
-- deriving via (PreludeEq AsyncException) instance PartialEq AsyncException
-- deriving via (PreludeEq ExitCode) instance PartialEq ExitCode
-- deriving via (PreludeEq IOErrorType) instance PartialEq IOErrorType
-- deriving via (PreludeEq IOException) instance PartialEq IOException
-- deriving via (PreludeEq HandlePosn) instance PartialEq HandlePosn
-- deriving via (PreludeEq BufferMode) instance PartialEq BufferMode
-- deriving via (PreludeEq Handle) instance PartialEq Handle
-- deriving via (PreludeEq Newline) instance PartialEq Newline
-- deriving via (PreludeEq NewlineMode) instance PartialEq NewlineMode
-- deriving via (PreludeEq IOMode) instance PartialEq IOMode
-- deriving via (PreludeEq Int) instance PartialEq Int
-- deriving via (PreludeEq Word) instance PartialEq Word
-- deriving via (PreludeEq Integer) instance PartialEq Integer
-- deriving via (PreludeEq Natural) instance PartialEq Natural
-- deriving via (PreludeEq ()) instance PartialEq ()
-- deriving via (PreludeEq Bool) instance PartialEq Bool
-- deriving via (PreludeEq Char) instance PartialEq Char
-- deriving via (PreludeEq Double) instance PartialEq Double
-- deriving via (PreludeEq Float) instance PartialEq Float

--------------------------------------------------------------------------

deriving newtype instance (PartialEq a) => PartialEq (Identity a)
deriving newtype instance (PartialEq a) => PartialEq (Product a)
deriving newtype instance (PartialEq a) => PartialEq (Sum a)

deriving anyclass instance (PartialEq a) => PartialEq [a]
deriving anyclass instance (PartialEq a) => PartialEq (NonEmpty a)
deriving anyclass instance (PartialEq a) => PartialEq (Maybe a)
deriving anyclass instance (PartialEq a, PartialEq b) => PartialEq (Either a b)

deriving anyclass instance (PartialEq a, PartialEq b) => PartialEq (a, b)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c) => PartialEq (a, b, c)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c, PartialEq d) => PartialEq (a, b, c, d)
deriving anyclass instance (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e) => PartialEq (a, b, c, d, e)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f)
    => PartialEq (a, b, c, d, e, f)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g)
    => PartialEq (a, b, c, d, e, f, g)
deriving anyclass instance
    (PartialEq a, PartialEq b, PartialEq c, PartialEq d, PartialEq e, PartialEq f, PartialEq g, PartialEq h)
    => PartialEq (a, b, c, d, e, f, g, h)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    )
    => PartialEq (a, b, c, d, e, f, g, h, i)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    , PartialEq n
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving anyclass instance
    ( PartialEq a
    , PartialEq b
    , PartialEq c
    , PartialEq d
    , PartialEq e
    , PartialEq f
    , PartialEq g
    , PartialEq h
    , PartialEq i
    , PartialEq j
    , PartialEq k
    , PartialEq l
    , PartialEq m
    , PartialEq n
    , PartialEq o
    )
    => PartialEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
