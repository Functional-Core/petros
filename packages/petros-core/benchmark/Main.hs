module Main where

import Bench.Petros.Eq
import Criterion.Main
import Prelude

main :: IO ()
main =
    defaultMain
        [ eq
        ]
