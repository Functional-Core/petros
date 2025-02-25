{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO:
-- Add laws
-- Add basic instances

module Petros.Eq
    ( PartialEq (..)
    , (~=)
    , (?=)
    , (/~=)
    , PartialHEq (..)
    , (~==)
    , (~==.)
    , (?==)
    , (?==.)
    , (/~==)
    , (/~==.)
    , Eq
    , (==)
    , (/=)
    , HEq
    , (===)
    , (===.)
    , (/==)
    , (/==.)
    ) where

import Prelude hiding (Eq (..))
import Prelude qualified (Eq (..))
import Data.Functor.Identity (Identity(..))
import GHC.Generics

class PartialEq a where
    eqPartial :: a -> a -> Bool
    default eqPartial :: (Generic a, PartialEq' (Rep a)) => a -> a -> Bool
    eqPartial x y = eqPartial' (from x) (from y)

    eqMaybe :: a -> a -> Maybe Bool
    default eqMaybe :: (Generic a, PartialEq' (Rep a)) => a -> a -> Maybe Bool
    eqMaybe x y = eqMaybe' (from x) (from y)

    neqPartial :: a -> a -> Bool
    neqPartial x y = not (eqPartial x y)

(~=) :: PartialEq a => a -> a -> Bool
(~=) = eqPartial

(?=) :: PartialEq a => a -> a -> Maybe Bool
(?=) = eqMaybe

(/~=) :: PartialEq a => a -> a -> Bool
(/~=) = neqPartial

class PartialHEq a b where
    heqPartial :: a -> b -> Bool
    default heqPartial :: (Generic a, Generic b, PartialHEq' (Rep a) (Rep b)) => a -> b -> Bool
    heqPartial x y = heqPartial' (from x) (from y)

    heqMaybe :: a -> b -> Maybe Bool
    default heqMaybe :: (Generic a, Generic b, PartialHEq' (Rep a) (Rep b)) => a -> b -> Maybe Bool
    heqMaybe x y = heqMaybe' (from x) (from y)

    hneqPartial :: a -> b -> Bool
    hneqPartial x y = not (heqPartial x y)

heqPartial_ :: PartialHEq a b => b -> a -> Bool
heqPartial_ = flip heqPartial

heqMaybe_ :: PartialHEq a b => b -> a -> Maybe Bool
heqMaybe_ = flip heqMaybe

hneqPartial_ :: PartialHEq a b => b -> a -> Bool
hneqPartial_ = flip hneqPartial

(~==) :: PartialHEq a b => a -> b -> Bool
(~==) = heqPartial

(~==.) :: PartialHEq a b => b -> a -> Bool
(~==.) = heqPartial_

(?==) :: PartialHEq a b => a -> b -> Maybe Bool
(?==) = heqMaybe

(?==.) ::PartialHEq a b => b -> a -> Maybe Bool
(?==.) = heqMaybe_

(/~==) :: PartialHEq a b => a -> b -> Bool
(/~==) = hneqPartial

(/~==.) :: PartialHEq a b => b -> a -> Bool
(/~==.) = hneqPartial_

class PartialEq a => Eq a where
    eq :: a -> a -> Bool
    
    neq :: a -> a -> Bool
    neq x y = not (eq x y)

(==) :: Eq a => a -> a -> Bool
(==) = eq

(/=) :: Eq a => a -> a -> Bool
(/=) = neq

class PartialHEq a b => HEq a b where
    heq :: a -> b -> Bool

    hneq :: a -> b -> Bool
    hneq x y = not (heq x y)

heq_ :: HEq a b => b -> a -> Bool
heq_ = flip heq

hneq_ :: HEq a b => b -> a -> Bool
hneq_ = flip hneq

(===) :: HEq a b => a -> b -> Bool
(===) = heq

(===.) :: HEq a b => b -> a -> Bool
(===.) = heq_

(/==) :: HEq a b => a -> b -> Bool
(/==) = hneq

(/==.) :: HEq a b => b -> a -> Bool
(/==.) = hneq_

newtype PreludeEq a = PreludeEq a
    deriving stock (Prelude.Eq)

instance Prelude.Eq a => PartialEq (PreludeEq a) where
    eqPartial = (Prelude.==)
    eqMaybe x y = Just $ (Prelude.==) x y

instance Prelude.Eq a => Eq (PreludeEq a) where
    eq = (Prelude.==)

------------------------- Generic Instances ------------------------- 

-- PartialEq'
class PartialEq' f where
    eqPartial' :: (f a) -> (f a) -> Bool
    eqMaybe' :: (f a) -> (f a) -> Maybe Bool

instance PartialEq' V1 where
    eqPartial' _ _ = undefined
    eqMaybe' _ _ = undefined

instance PartialEq' U1 where
    eqPartial' _ _ = True
    eqMaybe' _ _ = Just True

instance (PartialEq' f, PartialEq' g) => PartialEq' (f :+: g) where
    eqPartial' (L1 x) (L1 y) = eqPartial' x y
    eqPartial' (R1 x) (R1 y) = eqPartial' x y
    eqPartial' _ _ = False

    eqMaybe' (L1 x) (L1 y) = eqMaybe' x y
    eqMaybe' (R1 x) (R1 y) = eqMaybe' x y
    eqMaybe' _ _ = Just False

instance (PartialEq' f, PartialEq' g) => PartialEq' (f :*: g) where
    eqPartial' (x1 :*: y1) (x2 :*: y2) = (eqPartial' x1 x2) && (eqPartial' y1 y2)
    eqMaybe' (x1 :*: y1) (x2 :*: y2) = liftA2 (&&) (eqMaybe' x1 x2) (eqMaybe' y1 y2)

instance PartialEq c => PartialEq' (K1 i c) where
    eqPartial' (K1 x) (K1 y) = eqPartial x y
    eqMaybe' (K1 x) (K1 y) = eqMaybe x y

instance PartialEq' f => PartialEq' (M1 i t f) where
    eqPartial' (M1 x) (M1 y) = eqPartial' x y
    eqMaybe' (M1 x) (M1 y) = eqMaybe' x y

-- PartialHEq'
class PartialHEq' f g where
    heqPartial' :: (f a) -> (g a) -> Bool
    heqMaybe' :: (f a) -> (g a) -> Maybe Bool

instance PartialHEq' V1 g where
    heqPartial' _ _ = undefined
    heqMaybe' _ _ = undefined

instance PartialHEq' f V1 where
    heqPartial' _ _ = undefined
    heqMaybe' _ _ = undefined

instance PartialHEq' U1 U1 where
    heqPartial' _ _ = True
    heqMaybe' _ _ = Just True

instance PartialHEq' U1 g where
    heqPartial' _ _ = False
    heqMaybe' _ _ = Just False

instance PartialHEq' f U1 where
    heqPartial' _ _ = False
    heqMaybe' _ _ = Just False

-- TODO: Do we want to require the PartialEq' on f and g?
--
-- TODO: What about when f and g are reversed?
-- Either Int Bool, Either Bool Int
--
-- TODO: There are some interesting instances of partial equality with Either.
-- For example, Eq a => only creates PartialEq for Either a b
-- But Eq a, Eq b => creates full Eq (Either a b)
instance (PartialHEq' f g, PartialEq' f, PartialEq' g) => PartialHEq' (f :+: g) (f :+: g) where
   heqPartial' (L1 x) (L1 y) = eqPartial' x y
   heqPartial' (L1 x) (R1 y) = heqPartial' x y
   heqPartial' (R1 x) (L1 y) = heqPartial' y x
   heqPartial' (R1 x) (R1 y) = eqPartial' x y

   heqMaybe' (L1 x) (L1 y) = eqMaybe' x y
   heqMaybe' (L1 x) (R1 y) = heqMaybe' x y
   heqMaybe' (R1 x) (L1 y) = heqMaybe' y x
   heqMaybe' (R1 x) (R1 y) = eqMaybe' x y

instance PartialHEq' f g => PartialHEq' (f :*: f) (g :*: g) where
    heqPartial' (x1 :*: x2) (y1 :*: y2) = heqPartial' x1 y1 && heqPartial' x2 y2
    heqMaybe' (x1 :*: x2) (y1 :*: y2) = liftA2 (&&) (heqMaybe' x1 y1) (heqMaybe' x2 y2)

instance PartialHEq c d => PartialHEq' (K1 i c) (K1 j d) where
    heqPartial' (K1 x) (K1 y) = heqPartial x y
    heqMaybe' (K1 x) (K1 y) = heqMaybe x y

instance PartialHEq' f g => PartialHEq' (M1 i t f) (M1 j u g) where
    heqPartial' (M1 x) (M1 y) = heqPartial' x y
    heqMaybe' (M1 x) (M1 y) = heqMaybe' x y

-- Eq
class Eq' f where
    eq' :: f a -> f a -> Bool

instance Eq' V1 where
    eq' _ _ = undefined

instance Eq' U1 where
    eq' _ _ = True

instance (Eq' f, Eq' g) => Eq' (f :+: g) where
    eq' (L1 x) (L1 y) = eq' x y
    eq' (R1 x) (R1 y) = eq' x y
    eq' _ _ = False

instance (Eq' f, Eq' g) => Eq' (f :*: g) where
    eq' (x1 :*: y1) (x2 :*: y2) = eq' x1 x2 && eq' y1 y2

instance (Eq c) => Eq' (K1 i c) where
    eq' (K1 x) (K1 y) = eq x y

instance (Eq' f) => Eq' (M1 i t f) where
    eq' (M1 x) (M1 y) = eq' x y

-- HEq
class HEq' f g where
    heq' :: f a -> g a -> Bool


------------------------- Higher order instances ------------------------- 

deriving newtype instance PartialEq a => PartialEq (Identity a)

deriving anyclass instance PartialEq a => PartialEq (Maybe a)
deriving anyclass instance PartialEq a => PartialEq (Either a a)
deriving anyclass instance PartialEq a => PartialEq (a, a)

deriving anyclass instance (PartialHEq a b, PartialEq a, PartialEq b) => PartialHEq (Either a b) (Either a b)
deriving anyclass instance PartialHEq a b => PartialHEq (a, a) (b, b)

instance Eq a => Eq (Maybe a) where
instance (Eq a, Eq b) => Eq (Either a b) where

--------------------------------------------------------------------------

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

