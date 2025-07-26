{-# LANGUAGE Safe #-}

module Petros.NonZero ( NonZero (..) ) where

newtype NonZero a = NonZero { unwrap :: a }
