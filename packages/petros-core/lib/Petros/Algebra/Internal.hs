{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Petros.Algebra.Internal
    ( Semigroup (..)
    , Sum (..)
    , Product (..)
    , Monoid (..)
    , Group (..)
    , negate
    , recip
    , CommutativeGroup
    , Semiring (..)
    , Ring (..)
    , CommutativeRing
    , Domain
    , IntegralDomain
    , GcdDomain
    , EuclideanRing
    , GcdSemiring (..)
    , EuclideanSemiring (..)
    , DivisionRing (..)
    , Field
    , UnsafeSemiring (..)
    , UnsafeRing (..)
    , UnsafeEuclidean (..)
    , UnsafeField (..)
    ) where

import Data.Semigroup (Semigroup (..))
import Prelude (Show, Integer, (.)) 
import Prelude qualified
import Data.Either (fromRight)
    ( Real (..)
    , RealFrac (..)
    , Fractional (..)
    , Num (..)
    , Integral (..)
    , gcd
    , fromIntegral
    )
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Float (Double, Float)
import GHC.Natural (Natural)
import GHC.Real
    ( Rational
    , Ratio (..)
    )
import Data.Fixed (Fixed (..), HasResolution)
import Data.Monoid (Monoid (..))
import Data.Coerce (coerce)
import Petros.Eq
import Petros.Internal
import Data.Maybe (Maybe (..))
import GHC.Generics (Generic)
import Data.Either (Either (..))
import Data.Either.Extra (eitherToMaybe)
import GHC.Base (error, ($), Functor (..), otherwise)
import Data.String (IsString(..))
import Petros.Display (Display (..), displayStr)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Data.Tuple (fst, snd)

-----------------------------------------
-- Is Integer
-----------------------------------------

class FromInteger a where
    fromInteger :: Integer -> a

instance Prelude.Num a => FromInteger (FromPrelude a) where
    fromInteger = coerce @a . Prelude.fromInteger

deriving via (FromPrelude Int) instance FromInteger Int
deriving via (FromPrelude Int8) instance FromInteger Int8
deriving via (FromPrelude Int16) instance FromInteger Int16
deriving via (FromPrelude Int32) instance FromInteger Int32
deriving via (FromPrelude Int64) instance FromInteger Int64

deriving via (FromPrelude Word) instance FromInteger Word
deriving via (FromPrelude Word8) instance FromInteger Word8
deriving via (FromPrelude Word16) instance FromInteger Word16
deriving via (FromPrelude Word32) instance FromInteger Word32
deriving via (FromPrelude Word64) instance FromInteger Word64

deriving via (FromPrelude Integer) instance FromInteger Integer
deriving via (FromPrelude Natural) instance FromInteger Natural

deriving via (FromPrelude Float) instance FromInteger Float
deriving via (FromPrelude Double) instance FromInteger Double

instance (FromInteger a) => FromInteger (Ratio a) where
    fromInteger n = fromInteger n :% 1
    
instance (HasResolution a) => FromInteger (Fixed a) where
    fromInteger = Prelude.fromInteger

-----------------------------------------
-- Is Rational
-----------------------------------------

class FromRational a where
    fromRational :: Rational -> a

instance (Prelude.Fractional a) => FromRational (FromPrelude a) where
    fromRational = coerce @a . Prelude.fromRational 

deriving via (FromPrelude Float) instance FromRational Float
deriving via (FromPrelude Double) instance FromRational Double

instance (FromInteger a) => FromRational (Ratio a) where
    fromRational (n :% d) = fromInteger n :% fromInteger d

instance (HasResolution a) => FromRational (Fixed a) where
    fromRational = Prelude.fromRational

-----------------------------------------
-- Types
-----------------------------------------

newtype NonZero a = NonZero { getNonZero :: a }
    deriving stock (Show)

-- | 'Sum' and 'Product' are redefined in order to produce
-- a more coherent set of instances within the library.

-- | Newtype wrapper for Semigroups and Monoids over addition.
newtype Sum a = Sum { getSum :: a }
    deriving stock (Show)
    deriving newtype (PartialEq, Eq)

-- | Newtype wrapper for Semigroups and Monoids over multiplication.
newtype Product a = Product { getProduct :: a }
    deriving stock (Show)
    deriving newtype (PartialEq, Eq)

-----------------------------------------
-- Semigroup
-----------------------------------------

instance Semigroup (Sum Int) where
    (<>) = coerce ((Prelude.+) :: Int -> Int -> Int)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Int8) where
    (<>) = coerce ((Prelude.+) :: Int8 -> Int8 -> Int8)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Int16) where
    (<>) = coerce ((Prelude.+) :: Int16 -> Int16 -> Int16)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Int32) where
    (<>) = coerce ((Prelude.+) :: Int32 -> Int32 -> Int32)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Int64) where
    (<>) = coerce ((Prelude.+) :: Int64 -> Int64 -> Int64)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Word) where
    (<>) = coerce ((Prelude.+) :: Word -> Word -> Word)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Word8) where
    (<>) = coerce ((Prelude.+) :: Word8 -> Word8 -> Word8)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Word16) where
    (<>) = coerce ((Prelude.+) :: Word16 -> Word16 -> Word16)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Word32) where
    (<>) = coerce ((Prelude.+) :: Word32 -> Word32 -> Word32)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Word64) where
    (<>) = coerce ((Prelude.+) :: Word64 -> Word64 -> Word64)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Integer) where
    (<>) = coerce ((Prelude.+) :: Integer -> Integer -> Integer)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Natural) where
    (<>) = coerce ((Prelude.+) :: Natural -> Natural -> Natural)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Float) where
    (<>) = coerce ((Prelude.+) :: Float -> Float -> Float)
    {-# INLINE (<>) #-}

instance Semigroup (Sum Double) where
    (<>) = coerce ((Prelude.+) :: Double -> Double -> Double)
    {-# INLINE (<>) #-}

instance Semigroup (Product Int) where
    (<>) = coerce ((Prelude.*) :: Int -> Int -> Int)
    {-# INLINE (<>) #-}

instance Semigroup (Product Int8) where
    (<>) = coerce ((Prelude.*) :: Int8 -> Int8 -> Int8)
    {-# INLINE (<>) #-}

instance Semigroup (Product Int16) where
    (<>) = coerce ((Prelude.*) :: Int16 -> Int16 -> Int16)
    {-# INLINE (<>) #-}

instance Semigroup (Product Int32) where
    (<>) = coerce ((Prelude.*) :: Int32 -> Int32 -> Int32)
    {-# INLINE (<>) #-}

instance Semigroup (Product Int64) where
    (<>) = coerce ((Prelude.*) :: Int64 -> Int64 -> Int64)
    {-# INLINE (<>) #-}

instance Semigroup (Product Word) where
    (<>) = coerce ((Prelude.*) :: Word -> Word -> Word)
    {-# INLINE (<>) #-}

instance Semigroup (Product Word8) where
    (<>) = coerce ((Prelude.*) :: Word8 -> Word8 -> Word8)
    {-# INLINE (<>) #-}

instance Semigroup (Product Word16) where
    (<>) = coerce ((Prelude.*) :: Word16 -> Word16 -> Word16)
    {-# INLINE (<>) #-}

instance Semigroup (Product Word32) where
    (<>) = coerce ((Prelude.*) :: Word32 -> Word32 -> Word32)
    {-# INLINE (<>) #-}

instance Semigroup (Product Word64) where
    (<>) = coerce ((Prelude.*) :: Word64 -> Word64 -> Word64)
    {-# INLINE (<>) #-}

instance Semigroup (Product Integer) where
    (<>) = coerce ((Prelude.*) :: Integer -> Integer -> Integer)
    {-# INLINE (<>) #-}

instance Semigroup (Product Natural) where
    (<>) = coerce ((Prelude.*) :: Natural -> Natural -> Natural)
    {-# INLINE (<>) #-}

instance Semigroup (Product Float) where
    (<>) = coerce ((Prelude.*) :: Float -> Float -> Float)
    {-# INLINE (<>) #-}

instance Semigroup (Product Double) where
    (<>) = coerce ((Prelude.*) :: Double -> Double -> Double)
    {-# INLINE (<>) #-}

-----------------------------------------
-- Monoid
-----------------------------------------

instance Monoid (Sum Int) where
    mempty = Sum 0

instance Monoid (Sum Int8) where
    mempty = Sum 0

instance Monoid (Sum Int16) where
    mempty = Sum 0

instance Monoid (Sum Int32) where
    mempty = Sum 0

instance Monoid (Sum Int64) where
    mempty = Sum 0

instance Monoid (Sum Word) where
    mempty = Sum 0

instance Monoid (Sum Word8) where
    mempty = Sum 0

instance Monoid (Sum Word16) where
    mempty = Sum 0

instance Monoid (Sum Word32) where
    mempty = Sum 0

instance Monoid (Sum Word64) where
    mempty = Sum 0

instance Monoid (Sum Integer) where
    mempty = Sum 0

instance Monoid (Sum Natural) where
    mempty = Sum 0

instance Monoid (Sum Float) where
    mempty = Sum 0

instance Monoid (Sum Double) where
    mempty = Sum 0

instance Monoid (Product Int) where
    mempty = Product 1

instance Monoid (Product Int8) where
    mempty = Product 1

instance Monoid (Product Int16) where
    mempty = Product 1

instance Monoid (Product Int32) where
    mempty = Product 1

instance Monoid (Product Int64) where
    mempty = Product 1

instance Monoid (Product Word) where
    mempty = Product 1

instance Monoid (Product Word8) where
    mempty = Product 1

instance Monoid (Product Word16) where
    mempty = Product 1

instance Monoid (Product Word32) where
    mempty = Product 1

instance Monoid (Product Word64) where
    mempty = Product 1

instance Monoid (Product Integer) where
    mempty = Product 1

instance Monoid (Product Natural) where
    mempty = Product 1

instance Monoid (Product Float) where
    mempty = Product 1

instance Monoid (Product Double) where
    mempty = Product 1

-----------------------------------------
-- Group
-----------------------------------------

-- Monoid with an inverse element such that a <> inverse a == mempty
class Monoid a => Group a where
    inverse :: a -> a

negate :: forall a. (Group (Sum a)) => a -> a
negate = coerce (inverse :: Sum a -> Sum a)
{-# INLINE negate #-}
{-# SPECIALIZE negate :: Integer -> Integer #-}

recip :: forall a. (Group (Product a)) => a -> a
recip = coerce (inverse :: Product a -> Product a)
{-# INLINE recip #-}

instance Group (Sum Int) where
    inverse = coerce (Prelude.negate :: Int -> Int)
    {-# INLINE inverse #-}

instance Group (Sum Integer) where
    inverse = coerce (Prelude.negate :: Integer -> Integer)
    {-# INLINE inverse #-}

instance Group (Sum Double) where
    inverse = coerce (Prelude.negate :: Double -> Double)
    {-# INLINE inverse #-}

instance Group (Product Double) where
    inverse = coerce (Prelude.recip :: Double -> Double)
    {-# INLINE inverse #-}

-----------------------------------------
-- Commutative (Abelian) Group
-----------------------------------------

-- Also known as an Abelian Group
class (Group a) => CommutativeGroup a where
    -- a <> b = b <> a

instance CommutativeGroup (Sum Int)
instance CommutativeGroup (Sum Integer)
instance CommutativeGroup (Sum Double)
instance CommutativeGroup (Product Double)

-----------------------------------------
-- Semiring
-----------------------------------------

-- Types with both an additive and multiplicative monoid.
class (Monoid (Sum a), Monoid (Product a)) => Semiring a where
    zero :: a
    zero = coerce @(Sum a) mempty
    {-# INLINE zero #-}

    one :: a
    one = coerce @(Product a) mempty
    {-# INLINE one #-}

    (+) :: a -> a -> a
    (+) = coerce ((<>) @(Sum a))
    {-# INLINE (+) #-}

    (*) :: a -> a -> a
    (*) = coerce ((<>) @(Product a))
    {-# INLINE (*) #-}

instance Semiring Int
instance Semiring Word
instance Semiring Integer
instance Semiring Natural
instance Semiring Double

-----------------------------------------
-- Ring
-----------------------------------------

-- Semiring with additive negation
class (Semiring a, CommutativeGroup (Sum a)) => Ring a where
    (-) :: a -> a -> a
    x - y = x + (negate y)

instance Ring Int
instance Ring Integer
instance Ring Double

-----------------------------------------
-- Commutative Ring
-----------------------------------------

class (Ring a) => CommutativeRing a where
    -- a * b = b * a

instance CommutativeRing Integer
instance CommutativeRing Double

-----------------------------------------
-- GCD Semiring
-----------------------------------------

class (Semiring a) => GcdSemiring a where
    gcd :: a -> a -> a

instance GcdSemiring Int where
    gcd = (Prelude.gcd)

instance GcdSemiring Integer where
    gcd = (Prelude.gcd)

instance GcdSemiring Word where
    gcd = (Prelude.gcd)

instance GcdSemiring Natural where
    gcd = (Prelude.gcd)

-----------------------------------------
-- Euclidean Semiring
-----------------------------------------

class (GcdSemiring a) => EuclideanSemiring a where
    quot :: a -> NonZero a -> a
    quot x y = q where (q, _) = quotRem x y
    {-# INLINE quot #-}

    rem :: a -> NonZero a -> a
    rem x y = r where (_, r) = quotRem x y
    {-# INLINE rem #-}

    div :: a -> NonZero a -> a
    div x y = d where (d, _) = divMod x y
    {-# INLINE div #-}

    mod :: a -> NonZero a -> a
    mod x y = m where (_, m) = divMod x y
    {-# INLINE mod #-}

    quotRem :: a -> NonZero a -> (a, a)
    divMod :: a -> NonZero a -> (a, a)
    degree :: a -> Natural

-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
-- reduce ::  (EuclideanSemiring a) => a -> a -> Ratio a
-- {-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
-- reduce _ 0              =  ratioZeroDenominatorError
-- reduce x y              =  (x `quot` d) :% (y `quot` d)
--                            where d = NonZero (gcd x y)

instance EuclideanSemiring Int where 
    quotRem x y = Prelude.quotRem x (coerce y)
    divMod x y = Prelude.divMod x (coerce y)
    degree = Prelude.fromIntegral . Prelude.abs

-----------------------------------------
-- Division Ring (Skew Field)
-----------------------------------------

-- Also known as a Skew Field
class (Ring a) => DivisionRing a where
    (/) :: a -> NonZero a -> a

-- unsafeDiv :: forall a. (DivisionRing a, Eq a) => a -> a -> a
-- unsafeDiv x y
--     | y == zero = error "Divide by zero"
--     | otherwise = x / coerce y

instance DivisionRing Double where
    x / y = (Prelude./) x (coerce y)

-----------------------------------------
-- Domain
-----------------------------------------

class (Ring a) => Domain a where
    -- if a != 0 && b != 0 then a * b != 0

-----------------------------------------
-- Integral Domain
-----------------------------------------

type IntegralDomain a = (CommutativeRing a, Domain a)

-----------------------------------------
-- GCD Domain
-----------------------------------------

type GcdDomain a = (IntegralDomain a, GcdSemiring a)

-----------------------------------------
-- Euclidean (Ring) Domain
-----------------------------------------

type EuclideanRing a = (GcdDomain a, EuclideanSemiring a)

-----------------------------------------
-- Field
-----------------------------------------

type Field a = (DivisionRing a, EuclideanRing a)

-----------------------------------------
-- Unsafe
-----------------------------------------

data ArithmeticError
    = Overflow
    | Underflow
    | DivideByZero
    deriving stock (Show, Generic)
    deriving anyclass (PartialEq, Eq)

instance Display ArithmeticError where
    display Overflow = "arithmetic overflow error"
    display Underflow = "arithmetic underflow error"
    display DivideByZero = "divide by zero error"

type Checked a = Either ArithmeticError a

fromChecked :: HasCallStack => Checked a -> a
fromChecked = \case
    Right a -> a
    Left err -> withFrozenCallStack
        $ error (displayStr err)

class UnsafeSemiring a where
    checkedAdd :: a -> a -> Checked a
    checkedMul :: a -> a -> Checked a

    (+?) :: a -> a -> Maybe a 
    x +? y = eitherToMaybe (checkedAdd x y)
    
    -- | A default implementation which unwraps 'checkedAdd'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    (+!) :: HasCallStack => a -> a -> a
    x +! y = fromChecked (checkedAdd x y)

    (*?) :: a -> a -> Maybe a 
    x *? y = eitherToMaybe (checkedMul x y)

    -- | A default implementation which unwraps 'checkedMul'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    (*!) :: HasCallStack => a -> a -> a
    x *! y = fromChecked (checkedMul x y)

instance Semiring a => UnsafeSemiring a where
    checkedAdd x y = Right (x + y)
    {-# INLINE checkedAdd #-}

    checkedMul x y = Right (x * y)
    {-# INLINE checkedMul #-}

    x +? y = Just (x + y) 
    {-# INLINE (+?) #-}

    x *? y = Just (x * y)
    {-# INLINE (*?) #-}

    (+!) = (+)
    {-# INLINE (+!) #-}

    (*!) = (*)
    {-# INLINE (*!) #-}

class UnsafeRing a where
    checkedSub :: a -> a -> Checked a

    (-?) :: a -> a -> Maybe a 
    x -? y = eitherToMaybe (checkedSub x y)

    -- | A default implementation which unwraps 'checkedSub'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    (-!) :: HasCallStack => a -> a -> a
    x -! y = fromChecked (checkedSub x y)

instance Ring a => UnsafeRing a where
    checkedSub x y = Right (x - y)
    {-# INLINE checkedSub #-}

    x -? y = Just (x - y)
    {-# INLINE (-?) #-}

    x -! y = x - y
    {-# INLINE (-!) #-}

class UnsafeEuclidean a where
    checkedDivMod :: a -> a -> Checked (a, a)

    checkedIntDiv :: a -> a -> Checked a
    checkedIntDiv x y = fmap fst (checkedDivMod x y)

    checkedMod :: a -> a -> Checked a
    checkedMod x y = fmap snd (checkedDivMod x y)

    maybeDivMod :: a -> a -> Maybe (a, a)
    maybeDivMod x y = eitherToMaybe (checkedDivMod x y)

    maybeIntDiv :: a -> a -> Maybe a
    maybeIntDiv x y = fmap fst (maybeDivMod x y)

    maybeMod :: a -> a -> Maybe a
    maybeMod x y = fmap snd (maybeDivMod x y)
    
    -- | A default implementation which unwraps 'checkedDivMod'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    unsafeDivMod :: HasCallStack => a -> a -> (a, a)
    unsafeDivMod x y = fromChecked (checkedDivMod x y)

    -- | A default implementation which calls 'unsafeDivMod'
    -- is provided.
    unsafeIntDiv :: HasCallStack => a -> a -> a
    unsafeIntDiv x y = fst (unsafeDivMod x y)

    -- | A default implementation which calls 'unsafeDivMod'
    -- is provided.
    unsafeMod :: HasCallStack => a -> a -> a
    unsafeMod x y = snd (unsafeDivMod x y)

    -------------------------------------------------

    checkedQuotRem :: a -> a -> Checked (a, a)

    checkedQuot :: a -> a -> Checked a
    checkedQuot x y = fmap fst (checkedQuotRem x y)

    checkedRem :: a -> a -> Checked a
    checkedRem x y = fmap snd (checkedQuotRem x y)

    maybeQuotRem :: a -> a -> Maybe (a, a)
    maybeQuotRem x y = eitherToMaybe (checkedQuotRem x y)

    maybeQuot :: a -> a -> Maybe a
    maybeQuot x y = fmap fst (maybeQuotRem x y)

    maybeRem :: a -> a -> Maybe a
    maybeRem x y = fmap snd (maybeQuotRem x y)

    -- | A default implementation which unwraps 'checkedQuotRem'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    unsafeQuotRem :: HasCallStack => a -> a -> (a, a)
    unsafeQuotRem x y = fromChecked (checkedQuotRem x y)

    -- | A default implementation which calls 'unsafeQuotRem'
    -- is provided.
    unsafeQuot :: HasCallStack => a -> a -> a
    unsafeQuot x y = fst (unsafeQuotRem x y)

    -- | A default implementation which calls 'unsafeQuotRem'
    -- is provided.
    unsafeRem :: HasCallStack => a -> a -> a
    unsafeRem x y = snd (unsafeQuotRem x y)

    -------------------------------------------------
    
    checkedGcd :: a -> a -> Checked a
    
    maybeGcd :: a -> a -> Maybe a
    maybeGcd x y = eitherToMaybe (checkedGcd x y)

    -- | A default implementation which unwraps 'checkedDiv'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    unsafeGcd :: HasCallStack => a -> a -> a
    unsafeGcd x y = fromChecked (checkedGcd x y)

tryNonZero :: (Eq a, Semiring a) => a -> (NonZero a -> b) -> Checked b
tryNonZero x f
    | x == zero = Left DivideByZero
    | otherwise = Right (f (NonZero x))

instance (Eq a, EuclideanSemiring a) => UnsafeEuclidean a where
    checkedDivMod x y = tryNonZero y (divMod x)
    checkedIntDiv x y = tryNonZero y (div x)
    checkedMod x y = tryNonZero y (mod x)

    unsafeDivMod x y = x `divMod` (NonZero y)

    checkedQuotRem x y = tryNonZero y (quotRem x)
    checkedQuot x y = tryNonZero y (quot x)
    checkedRem x y = tryNonZero y (rem x)

    unsafeQuotRem x y = x `quotRem` (NonZero y)

    checkedGcd x y = Right (gcd x y)
    {-# INLINE checkedGcd #-}
    maybeGcd x y = Just (gcd x y)
    {-# INLINE maybeGcd #-}
    unsafeGcd x y = gcd x y
    {-# INLINE unsafeGcd #-}

class UnsafeField a where
    checkedDiv :: a -> a -> Checked a

    maybeDiv :: a -> a -> Maybe a
    maybeDiv x y = eitherToMaybe (checkedDiv x y)

    -- | A default implementation which unwraps 'checkedDiv'
    -- is provided but it is expected that instances overwrite
    -- this with a raw-unchecked variant for efficiency.
    unsafeDiv :: HasCallStack => a -> a -> a
    unsafeDiv x y = fromChecked (checkedDiv x y)

instance (Field a, Eq a) => UnsafeField a where
    checkedDiv x y = tryNonZero y (x /)
    unsafeDiv x y = x / NonZero y

