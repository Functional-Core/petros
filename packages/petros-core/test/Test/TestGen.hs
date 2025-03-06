{-# LANGUAGE TemplateHaskell #-}

module Test.TestGen where

import Control.Monad.Reader (ReaderT (..))
import Language.Haskell.TH
import Test.Law
import Prelude
import Test.MonoType
import Data.Maybe (mapMaybe)

type TestGen a = ReaderT TestGenEnv Q a

runTestGen :: TestGenEnv -> TestGen a -> Q a
runTestGen env x = runReaderT x env

type SpecLabel = String

data TestGenEnv = TestGenEnv
    { typeCandidates :: [MonoType]
    , -- Such as Arbitrary and Show
      candidateInsts :: [Name]
    , -- how many successful candidates to
      -- generate tests for (Nothing == all)
      candidatesLimit :: Maybe Int
    , genPropertyTest :: [Type] -> Law -> TestGen Exp
    , genSpec :: SpecLabel -> [Exp] -> TestGen (Maybe Exp)
    }

mkCandidates :: [Name] -> [MonoType]
mkCandidates = mapMaybe (mkMonoType . ConT)

mkDefaultEnv
    :: ([Type] -> Law -> TestGen Exp)
    -> (SpecLabel -> [Exp] -> TestGen (Maybe Exp))
    -> [Name]
    -> TestGenEnv
mkDefaultEnv pg sg insts =
    TestGenEnv
        { typeCandidates = mkCandidates [''Int, ''Bool] --, ''String]
        , candidatesLimit = Just 1
        , candidateInsts = insts
        , genPropertyTest = pg
        , genSpec = sg
        }
