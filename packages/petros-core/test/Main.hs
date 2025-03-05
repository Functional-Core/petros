{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude hiding (Eq (..))
import Test.Petros.Eq
import Test.Hspec
import Language.Haskell.TH
import Petros.Eq
import Control.Arrow (Arrow)

main :: IO ()
main = do
    putStrLn "........"
    putStrLn $(stringE . show =<< testClass ''Eq)
    -- putStrLn $(stringE . show =<< testDummy)
    -- putStrLn $(stringE . show =<< testEq)
    putStrLn "........"
    -- putStrLn $(stringE . show =<< testReify)
    -- putStrLn $(stringE . show =<< testEq)
    -- hspec $(runTestGenClass testEnv ''Eq)
    putStrLn "Test suite not yet implemented"

