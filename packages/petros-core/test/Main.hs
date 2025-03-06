{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Main (main) where

import Control.Arrow (Arrow)
import Language.Haskell.TH
import Petros.Eq
import Test.Generators (runTestGenClass)
import Test.Hspec
import Test.Petros.Eq
import Test.QuickCheck
import Prelude hiding (Eq (..))
import Control.Monad.Identity (Identity)
import Data.Semigroup
import Petros.Eq.PartialEq qualified
import Test.Hspec.QuickCheck

main :: IO ()
main = do
    -- putStrLn "........"
    -- putStrLn $(stringE . show =<< testClass ''Dummy)
    -- putStrLn $(stringE . show =<< testInsts ''Dummy)
    -- putStrLn $(stringE . show =<< test)
    -- putStrLn "........"
    -- putStrLn $(stringE . pprint =<< runTestGenClass testEnv ''PartialEq)
    hspec $(runTestGenClass testEnv ''PartialEq)
    putStrLn "Test suite not yet implemented"
