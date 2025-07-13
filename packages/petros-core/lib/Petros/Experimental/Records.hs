{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Trustworthy #-}

module Petros.Experimental.Records
    ( OptionChain (..)
    ) where

import Prelude
import GHC.Records ( HasField(..) )
import GHC.OverloadedLabels ( IsLabel(..) )

newtype Sel r a = Sel { _select :: r -> a }

instance HasField l r a => IsLabel l (Sel r a) where
  fromLabel = Sel (getField @l)

class Monad m => OptionChain m where
    (.?) :: m r -> Sel r a -> m a
    mr .? (Sel select) = fmap select mr

    (.??) :: m r -> Sel r (m a) -> m a
    mr .?? (Sel select) = mr >>= select

instance OptionChain Maybe
instance OptionChain (Either e)

-- data Outer = Outer { inner :: Maybe Inner }
-- data Inner = Inner { value :: Int, deeper :: Maybe Deeper }
-- data Deeper = Deeper { another :: Maybe Int }
--
-- getValue :: Outer -> Maybe Int
-- getValue outer = outer.inner .? #value
--
-- getOtherValue :: Outer -> Maybe Int
-- getOtherValue outer = outer.inner .?? #deeper .?? #another
