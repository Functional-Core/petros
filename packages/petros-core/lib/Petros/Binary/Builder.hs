{-# LANGUAGE Safe #-}

module Petros.Binary.Builder (
    module Data.ByteString.Builder
) where

-- IO operations will be provided lifted/unlifted in Petros.IO
import Data.ByteString.Builder hiding (hPutBuilder, writeFile)
