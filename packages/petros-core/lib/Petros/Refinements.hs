{-# LANGUAGE Safe #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Petros.Refinements where

import Data.Kind (Type)
import Data.Text (Text)
import Prelude
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics
import GHC.TypeLits (Nat)

{- Goals:
 - 1) Composable refinements
 - 2) Permit use of instance ops such as (+) :: Positive -> Positive -> Positive
 - 3) Little to no runtime overhead
 - 
 - Current solutions:
 - 1) Newtypes:
 -      not composable e.g. (Whole (Positive a)) is not (Positive (Whole a))
 -      derived instances circumvent constructors e.g. RealFrac.fromRational
 - 2) Refined:
 -      not composable again
 -      can't use with derived classes
 -
 -
 - -}

newtype Refinement ps a = Refinement { inner :: a }

type RefinementError = Text

-- class Typeable p => Predicate p a where
--     validate :: Proxy p -> a -> Maybe RefinementError

data Positive
    deriving stock (Generic)

data Whole
    deriving stock (Generic)

data GreaterThan (n :: Nat)
    deriving stock (Generic)

data Between (l :: Nat) (u :: Nat)
    deriving stock (Generic)

type Predicate = Type

-- instance (Num a, Ord a) => Predicate Positive a where
--     validate _ n
--         | n > 0 = Nothing
--         | otherwise = Just "Not positive"
--
-- instance (RealFrac a, Eq a) => Predicate Whole a where
--     validate _ n
--         | snd (properFraction @_ @Integer n) == 0 = Nothing
--         | otherwise = Just "Not a whole number"

class Member (p :: Predicate) (ps :: [Predicate]) where

class Validate (p :: Predicate) a where
    validate :: a -> Maybe RefinementError

instance (Num a, Ord a) => Validate Positive a where
    validate n
        | n > 0 = Nothing
        | otherwise = Just "Not positive"

refine :: forall p ps a. (Validate p a, Member p ps) => a -> Either RefinementError (Refinement ps a)
refine x = maybe (Right (Refinement x)) Left (validate @p x)

unrefine :: forall ps a. Refinement ps a -> a
unrefine x = x.inner

