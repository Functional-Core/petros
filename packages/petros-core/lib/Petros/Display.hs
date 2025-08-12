{-# LANGUAGE Safe #-}

module Petros.Display
    ( Display (..)
    , displayStr
    ) where

import Data.Text (Text)
import Data.String (String)
import qualified Data.Text as Text
import Data.Function ((.))

class Display a where
    display :: a -> Text

displayStr :: Display a => a -> String
displayStr = Text.unpack . display
