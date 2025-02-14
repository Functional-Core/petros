{-# LANGUAGE Safe #-}

-- BOB: Bob Optimises Builders
module Petros.Experimental.Bob
    ( Builder (..)
    , DList
    , builder
    , (.<>)
    , (<>.)
    , runBuilder
    ) where

import Data.Function (($))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))

newtype Builder a = Builder (a -> a)
type DList a = Builder [a]

builder :: (Semigroup a) => a -> Builder a
builder x = Builder $ \y -> x <> y

(.<>) :: (Monoid a) => Builder a -> a -> Builder a
(.<>) (Builder f) x = Builder $ \y -> f (x <> y)

(<>.) :: (Monoid a) => a -> Builder a -> Builder a
(<>.) x (Builder f) = Builder $ \y -> x <> (f y)

-- Good: xs ++ (ys ++ zs)
-- Bad: (xs ++ ys) ++ zs

-- (.<>) is good
-- (\nxt -> xs <> nxt) .<> ys .<> zs
-- = (\nxt' -> (\nxt -> xs <> nxt) (ys <> nxt')) .<> zs
-- = (\nxt'' -> (\nxt' -> (\nxt -> xs <> nxt) (ys <> nxt')) (zs <> nxt''))
-- apply to "" = (\nxt' -> (\nxt -> xs <> nxt) (ys <> nxt')) (zs <> ""))
-- (\nxt -> xs <> nxt) (ys <> (zs <> ""))
-- xs <> (ys <> (zs <> ""))

-- (<>.) is good
-- xs .<> ys .<> (\nxt -> zs <> nxt)
-- xs .<> (\nxt' -> ys <> (\nxt -> zs <> nxt) (nxt'))
-- \nxt'' -> xs <> ((\nxt' -> ys <> (\nxt -> zs <> nxt) nxt') nxt'')
-- apply to "" = xs <> ((\nxt' -> ys <> (\nxt -> zs <> nxt) nxt') "")
-- = xs (ys <> (\nxt -> zs <> nxt) "")
-- = xs (ys <> (zs <> ""))

-- The only problem is that because we have to join a `Builder a`
-- with `a` we can't apply join strictly and hence we get this
-- deep nesting of lambdas. That said, the whole point of a DList
-- is not to join strictly, if we joined strictly we would get
-- the poor performance of a foldl style (++).

-- We should profile this implementation against the existing DList library,
-- and against Text.Lazy.Builder

-- An important thing to note is that use of `(<>)` or types
-- such as `Text` and `ByteString` requires reallocating each time.
-- Instead we should lazily collect our elements into a list
-- then run `mconcat` to make use of specialised implementations
-- for the aforementioned types.

runBuilder :: (Monoid a) => Builder a -> a
runBuilder (Builder f) = f mempty
