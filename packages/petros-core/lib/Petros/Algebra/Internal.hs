{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Petros.Algebra.Internal
    ( Semigroup (..)
    , Sum (..)
    , Product (..)
    , Monoid (..)
    , Group (..)
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
    , (%)
    ) where

import Data.Semigroup (Semigroup (..))
import Prelude (Show, Integer, (.)) 
import Prelude qualified
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
    , Ratio (..), ratioZeroDenominatorError
    )
import Data.Fixed (Fixed (..), HasResolution)
import Data.Monoid (Monoid (..))
import Data.Coerce (coerce)
import Petros.Eq
import Petros.Internal
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)

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
    {-# INLINE mempty #-}

instance Monoid (Sum Int8) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Int16) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Int32) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Int64) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Word) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Word8) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Word16) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Word32) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Word64) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Integer) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Natural) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Float) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Sum Double) where
    mempty = Sum 0
    {-# INLINE mempty #-}

instance Monoid (Product Int) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Int8) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Int16) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Int32) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Int64) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Word) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Word8) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Word16) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Word32) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Word64) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Integer) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Natural) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Float) where
    mempty = Product 1
    {-# INLINE mempty #-}

instance Monoid (Product Double) where
    mempty = Product 1
    {-# INLINE mempty #-}

-----------------------------------------
-- Group
-----------------------------------------

-- Monoid with an inverse element such that a <> inverse a == mempty
class Monoid a => Group a where
    inverse :: a -> a

instance Group (Sum Int) where
    inverse = coerce (Prelude.negate :: Int -> Int)
    {-# INLINE inverse #-}

instance Group (Sum Integer) where
    inverse = coerce (Prelude.negate :: Integer -> Integer)
    {-# INLINE inverse #-}

instance Group (Sum Double) where
    inverse = coerce ((Prelude.*) (-1) :: Double -> Double)
    {-# INLINE inverse #-}

instance Group (Product Double) where
    inverse = coerce ((Prelude.recip) :: Double -> Double)
    {-# INLINE inverse #-}

class AdditiveGroup a where
    negate :: a -> a

class MultiplicativeGroup a where
    recip :: a -> a

instance Group (Sum a) => AdditiveGroup a where
    -- TODO: Add specialisations
    negate = coerce (inverse :: Sum a -> Sum a)
    {-# INLINE negate #-}

instance Group (Product a) => MultiplicativeGroup a where
    -- TODO: Add specialisations
    recip = coerce (inverse :: Product a -> Product a)
    {-# INLINE recip #-}

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

    rem :: a -> NonZero a -> a
    rem x y = r where (_, r) = quotRem x y

    quotRem :: a -> NonZero a -> (a, a)
    degree :: a -> Natural

(%) :: forall a. EuclideanSemiring a => a -> a -> a
x % y = rem x (coerce y)
{-# INLINE (%) #-}

-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
-- reduce ::  (EuclideanSemiring a) => a -> a -> Ratio a
-- {-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
-- reduce _ 0              =  ratioZeroDenominatorError
-- reduce x y              =  (x `quot` d) :% (y `quot` d)
--                            where d = NonZero (gcd x y)

instance EuclideanSemiring Int where 
    quot x y = Prelude.quot x (coerce y)
    rem x y = Prelude.rem x (coerce y)
    quotRem x y = Prelude.quotRem x (coerce y)
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













