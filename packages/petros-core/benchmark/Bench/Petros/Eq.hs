{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

module Bench.Petros.Eq (eq) where

import Criterion.Main
import GHC.Generics
import Petros.Eq
import Prelude hiding (Eq (..))
import Prelude qualified

one :: Int
one = 1

two :: Int
two = 2

data Thing = Thing
    { field1 :: Int
    , field2 :: Maybe Int
    , field3 :: Bool
    }
    deriving stock (Generic, Prelude.Eq)
    deriving anyclass (HetPartialEq Thing, HetEq Thing)

thing1, thing2 :: Thing
thing1 = Thing 123456 (Just 21) True
thing2 = Thing 123456 Nothing False

deep :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int Bool)))))
deep = Right . Right . Right . Right . Right . Right $ True

eq :: Benchmark
eq =
    bgroup
        "Eq"
        [ bgroup
            "prelude"
            [ bench "1 == 1" $ whnf (Prelude.== one) one
            , bench "1 /= 2" $ whnf (Prelude./= one) two
            , bench "[1..1000]" $ whnf (Prelude.== [one .. 1000]) [one .. 1000]
            , bench "[1..10000]" $ whnf (Prelude.== [one .. 10000]) [one .. 10000]
            , bench "Thing" $ whnf (Prelude.== thing1) thing2
            , bench "Deep" $ whnf (Prelude.== deep) deep
            ]
        , bgroup
            "Petros.Eq"
            [ bench "1 == 1" $ whnf (== one) one
            , bench "1 /= 2" $ whnf (/= one) two
            , bench "[1..1000]" $ whnf (== [one .. 1000]) [one .. 1000]
            , bench "[1..10000]" $ whnf (== [one .. 10000]) [one .. 10000]
            , bench "Thing" $ whnf (== thing1) thing2
            , bench "Deep" $ whnf (~== deep) deep
            ]
        ]
