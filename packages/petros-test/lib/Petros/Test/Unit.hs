{-# LANGUAGE Trustworthy #-}

module Petros.Test.Unit
    ( shouldBe
    , shouldNotBe
    , shouldReturn
    , shouldNotReturn
    , shouldSatisfy
    , shouldNotSatisfy
    , shouldStartWith
    , shouldEndWith
    , shouldContain
    , shouldNotContain
    , shouldMatchList
    , shouldThrow

    , Expectation
    , expectationFailure

    , Selector
    , anyException
    , anyErrorCall
    , anyIOException
    , anyArithException
    , errorCall
    ) where

import Test.Hspec

-- tools for defining unit tests

