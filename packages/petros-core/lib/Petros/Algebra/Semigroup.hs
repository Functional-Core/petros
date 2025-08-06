{-# LANGUAGE Safe #-}

module Petros.Algebra.Semigroup
    ( Semigroup (..)
    , Sum (..)
    , Product (..)
    ) where

import Petros.Algebra.Internal

-- TODO: Do we bother re-exporting All/Any for Bool?
-- and even the others like First/Last?
-- 
-- I'm on the fence about whether I like the more
-- "creative" ones like First/Last.
--
-- All/Any just seem like Sum/Product, perhaps the
-- more specific naming is still useful though?
