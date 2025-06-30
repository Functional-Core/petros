{-# LANGUAGE Trustworthy #-}

module Petros.Numeric.Positive where

import Refined (Refined, Predicate, unrefine)
import Refined qualified as R
import Refined.Unsafe (reallyUnsafeRefine)

import Petros.Numeric.Primitives
import Data.Tuple (snd)
import Data.Eq ((==))
import Data.Maybe (Maybe(..))
import Data.Bool (otherwise)
import Data.Data (typeRep)
import Prelude (Show, Read, ($))

newtype Positive a = Positive { inner :: Refined R.Positive a }
    deriving newtype (Show, Read)

-- instance Num a => Num (Positive a) where
--   (+) x y = Positive $ reallyUnsafeRefine (unrefine x.inner + unrefine y.inner)
--   (*) = _
--   abs = _
--   signum = _
--   fromInteger = _
--   negate = _
    
data Whole_

instance RealFrac a => Predicate Whole_ a where
    validate p n
        | snd (properFraction @_ @Integer n) == 0 = Nothing
        | otherwise = R.throwRefineOtherException
            (typeRep p)
            "Not a whole number"

-- instance Integral a => Predicate Whole_ a where
--     validate _ _ = Nothing

newtype Whole a = Whole { inner :: Refined Whole_ a }
    deriving newtype (Show, Read)

type WholePositive a = Whole (Positive a)

-- Whole (Positive a) =/= Positive (Whole a) :(


