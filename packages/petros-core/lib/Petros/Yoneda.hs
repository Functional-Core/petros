module Petros.Yoneda where

import Prelude

-- fmap :: (a -> b) -> f a -> f b
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor f => Functor (Yoneda f) where
    fmap :: (a -> b) -> Yoneda f a -> Yoneda f b
    fmap f (Yoneda run) = Yoneda \g -> run (g . f)

toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda \f -> fmap f fa

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda run) = run id


