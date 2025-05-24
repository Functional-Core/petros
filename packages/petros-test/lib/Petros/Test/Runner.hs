{-# LANGUAGE Trustworthy #-}

module Petros.Test.Runner
    ( hspec
    , hspecWith
    , defaultConfig
    , Config (..)
    , before
    , after
    , around
    , before_
    , after_
    , around_
    , pending
    , pendingWith
    , beforeAll
    , beforeAll_
    , beforeAllWith
    , afterAll
    , afterAll_
    , aroundAll
    , aroundAll_
    , aroundAllWith
    ) where

-- Test runners and configuration

import Test.Hspec
import Test.Hspec.Runner
