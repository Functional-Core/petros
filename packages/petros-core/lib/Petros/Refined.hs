{-# LANGUAGE Trustworthy #-}

module Petros.Refined where

import Refined (Refined, unrefine)
import Refined.Unsafe (reallyUnsafeRefine)
import qualified Refined as R

import Petros.Algebra.Semigroup
import Prelude (($))

type Positive a = Refined R.Positive a

instance Semigroup a => Semigroup (Positive a) where
  (<>) x y = reallyUnsafeRefine (unrefine x <> unrefine y)

-- Downside, orphan instance

newtype NewPositive a = NewPositive { inner :: Refined R.Positive a }

instance Semigroup a => Semigroup (NewPositive a) where
  (<>) x y = NewPositive $ reallyUnsafeRefine (unrefine x.inner <> unrefine y.inner)

-- Downside, another layer of wrapping,
-- defeats the point of using Refined really, may as well use smart constructors

-- Essentially we want to define our own `Refined` type and be able
-- to control the instances without orphans.
--
-- Alternatively we could accept orphans within the core library and export them.
--
-- We could also define our own `Refined` type and define the
-- instances in the same module.
--
-- This wouldn't help with modules which define new predicates
-- though.

data Whole

type Whole_ a = Refined Whole a

instance Semigroup a => Semigroup (Whole_ a) where
  (<>) x y = reallyUnsafeRefine (unrefine x <> unrefine y)

-- Modules that define a new predicate seem to still be able to
-- define their own instances without being considered orphan.
--
-- That makes sense.

-- So it seems defining our own predicates with appropriate
-- instances is worth doing.
--
-- Likewise, redefining Refined to enable composing predicates.
--
-- Having 'Refined' take a list of predicates works under conjunction (AND),
-- but does not solve the issue when it comes to disjunction (OR).
-- The refined library provides And, Or, Xor.
-- 
-- Creating a type-level list also causes issues with instance resolution.
-- For example
-- instance (Member Positive ps, Semigroup a) => Semigroup (Refined ps a) where...
-- instance (Member Whole ps, Semigroup a) => Semigroup (Refined ps a) where...
-- 
-- these instances overlap
-- even if we allow it, GHC won't know what to do if ps contains both Positive and Whole...
--
-- From Alexis King's blog:
-- In libraries, the newtype-afforded notion of safety via encapsulation is useful, as libraries
-- often provide the building blocks used to construct more complicated data structures.
-- Such libraries generally receive more scrutiny and care than application code does,
-- especially given they change far less frequently. In application code, these techniques
-- are still useful, but the churn of a production codebase tends to weaken encapsulation boundaries
-- over time, so correctness by construction should be preferred whenever practical.
--
-- What do we think about the tagging approach such as `Id "UserAccount"`?
