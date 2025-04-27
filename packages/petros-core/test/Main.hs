module Main (main) where

import Spec qualified
import Test.Hspec.Runner
import Prelude

main :: IO ()
main = hspecWith config Spec.spec
    where
        config = defaultConfig{configQuickCheckMaxSuccess = Just 10000}
