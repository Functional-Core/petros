{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

module Petros.Ord.Lex
    ( Lex (..)
    , cmpPartial
    , (<.)
    , (<=.)
    , (>.)
    , (>=.)
    , (>/<)
    , cmp
    , (<)
    , (<=)
    , (>)
    , (>=)
    , min
    , max
    , LexPartialOrd
    , LexOrd
    ) where

import Control.Applicative ((<|>))
import GHC.Generics
import Petros.Eq
import Petros.Ord.Ord (Ord)
import Petros.Ord.Ord qualified as Ord
import Petros.Ord.PartialOrd (PartialOrd)
import Petros.Ord.PartialOrd qualified as Ord
import Prelude hiding (Eq (..), Ord (..))

--------------------------------------------------
-- Lexicographical ordering
--------------------------------------------------

class GLexPartialOrd f where
    gcmpLexPartial :: f a -> f a -> Maybe Ordering

instance GLexPartialOrd V1 where
    gcmpLexPartial = undefined

instance GLexPartialOrd U1 where
    gcmpLexPartial _ _ = Just EQ

instance (GLexPartialOrd f, GLexPartialOrd g) => GLexPartialOrd (f :+: g) where
    gcmpLexPartial (L1 x) (L1 y) = gcmpLexPartial x y
    gcmpLexPartial (R1 x) (R1 y) = gcmpLexPartial x y
    gcmpLexPartial _ _ = Nothing

instance (GLexPartialOrd f, GLexPartialOrd g) => GLexPartialOrd (f :*: g) where
    gcmpLexPartial (x1 :*: y1) (x2 :*: y2) =
        gcmpLexPartial x1 x2 <|> gcmpLexPartial y1 y2

instance (PartialOrd c) => GLexPartialOrd (K1 i c) where
    gcmpLexPartial (K1 x) (K1 y) = Ord.cmpPartial x y

instance (GLexPartialOrd c) => GLexPartialOrd (M1 i j c) where
    gcmpLexPartial (M1 x) (M1 y) = gcmpLexPartial x y

class GLexOrd f where
    gcmpLex :: f a -> f a -> Ordering

instance GLexOrd V1 where
    gcmpLex = undefined

instance GLexOrd U1 where
    gcmpLex _ _ = EQ

-- Note we cannot provide a total ordering for sum types
-- since we cannot compare lefts with rights.

instance (GLexOrd f, GLexOrd g) => GLexOrd (f :*: g) where
    gcmpLex (x1 :*: y1) (x2 :*: y2) =
        if ordx == EQ then ordy else ordx
        where
            ordx = gcmpLex x1 x2
            ordy = gcmpLex y1 y2

instance (Ord c) => GLexOrd (K1 i c) where
    gcmpLex (K1 x) (K1 y) = Ord.cmp x y

instance (GLexOrd c) => GLexOrd (M1 i j c) where
    gcmpLex (M1 x) (M1 y) = gcmpLex x y

newtype Lex a = Lex {unlex :: a}
    deriving stock (Generic)
    deriving newtype (Show, PartialEq, Eq)

instance (PartialEq a, Generic a, GLexPartialOrd (Rep a)) => PartialOrd (Lex a) where
    cmpPartial (Lex x) (Lex y) = gcmpLexPartial (from x) (from y)

instance (Eq a, Generic a, GLexPartialOrd (Rep a), GLexOrd (Rep a)) => Ord (Lex a) where
    cmp (Lex x) (Lex y) = gcmpLex (from x) (from y)

type LexPartialOrd a = (PartialEq a, Generic a, GLexPartialOrd (Rep a))

liftLex :: (Lex a -> Lex a -> b) -> a -> a -> b
liftLex op x y = (Lex x) `op` (Lex y)
{-# INLINE liftLex #-}

(<.), (<=.), (>.), (>=.), (>/<) :: (LexPartialOrd a) => a -> a -> Bool
(<.) = liftLex (Ord.<.)
(<=.) = liftLex (Ord.<=.)
(>.) = liftLex (Ord.>.)
(>=.) = liftLex (Ord.>=.)
(>/<) = liftLex (Ord.>/<)
{-# INLINE (<.) #-}
{-# INLINE (<=.) #-}
{-# INLINE (>.) #-}
{-# INLINE (>=.) #-}
{-# INLINE (>/<) #-}

cmpPartial :: (LexPartialOrd a) => a -> a -> Maybe Ordering
cmpPartial = liftLex Ord.cmpPartial
{-# INLINE cmpPartial #-}

type LexOrd a = (Eq a, Generic a, GLexOrd (Rep a), GLexPartialOrd (Rep a))

(<), (<=), (>), (>=) :: (LexOrd a) => a -> a -> Bool
(<) = liftLex (Ord.<)
(<=) = liftLex (Ord.<=)
(>) = liftLex (Ord.>)
(>=) = liftLex (Ord.>=)
{-# INLINE (<) #-}
{-# INLINE (<=) #-}
{-# INLINE (>) #-}
{-# INLINE (>=) #-}

min, max :: (LexOrd a) => a -> a -> a
min x y = unlex $ liftLex (Ord.min) x y
max x y = unlex $ liftLex (Ord.max) x y
{-# INLINE min #-}
{-# INLINE max #-}

cmp :: (LexOrd a) => a -> a -> Ordering
cmp = liftLex Ord.cmp
