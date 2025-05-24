{-# LANGUAGE Trustworthy #-}

module Petros.Test.Property
    ( property
    , prop
    , Property
    , modifyMaxSuccess
    , modifyMaxSize
    , modifyMaxDiscardRatio
    , modifyArgs
    , modifyMaxShrinks
    , Args (..)
    ) where

import Test.Hspec.QuickCheck
import Test.QuickCheck

-- tools for defining property tests

