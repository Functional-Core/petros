{-# LANGUAGE Trustworthy #-}

module Petros.Algebra.EuclideanSemiring
    ( EuclideanSemiring (..)
    , (%)
    ) where

import Petros.Algebra.GcdSemiring (GcdSemiring (..))
import GHC.Natural (Natural)
import Petros.NonZero (NonZero (..))
import Data.Coerce (coerce)
import GHC.Integer (Integer)
import Data.Ratio (Ratio)
import GHC.Real (Rational, Ratio (..), ratioZeroDenominatorError)

class (GcdSemiring a) => EuclideanSemiring a where
    quot :: a -> NonZero a -> a
    rem :: a -> NonZero a -> a
    quotRem :: a -> a -> (a, a)
    degree :: a -> Natural

(%) :: forall a. EuclideanSemiring a => a -> a -> a
x % y = rem x (coerce y)
{-# INLINE (%) #-}

-- -- | 'reduce' is a subsidiary function used only in this module.
-- -- It normalises a ratio by dividing both numerator and denominator by
-- -- their greatest common divisor.
-- reduce ::  (EuclideanSemiring a) => a -> a -> Ratio a
-- {-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
-- reduce _ 0              =  ratioZeroDenominatorError
-- reduce x y              =  (x `quot` d) :% (y `quot` d)
--                            where d = NonZero (gcd x y)
