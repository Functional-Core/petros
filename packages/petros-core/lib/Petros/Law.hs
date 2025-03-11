{-# LANGUAGE Safe #-}

module Petros.Law
    ( (===>)
    , (<==>)
    , idempotent
    , reflexive
    , antireflexive
    , transitive
    , transitive_
    , symmetric
    , symmetric_
    , antisymmetric
    , leftIdentity
    , rightIdentity
    , identity
    , associative
    , commutative
    , commutative_
    , inverse
    , dual
    , involution
    ) where

import Prelude

(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a

idempotent :: Prelude.Eq a => (a -> a) -> a -> Bool
idempotent f x = f x == f (f x)

reflexive :: (a -> a -> Bool) -> a -> Bool
reflexive f x = f x x

antireflexive :: (a -> a -> Bool) -> a -> Bool
antireflexive f x = not (f x x)

transitive :: (a -> b -> Bool) -> (b -> c -> Bool) -> (a -> c -> Bool) -> a -> b -> c -> Bool
transitive f g h a b c = f a b && g b c ===> h a c

transitive_ :: (a -> a -> Bool) -> a -> a -> a -> Bool
transitive_ f = transitive f f f

symmetric :: (a -> b -> Bool) -> (b -> a -> Bool) -> a -> b -> Bool
symmetric f g a b = f a b <==> g b a

symmetric_ :: (a -> a -> Bool) -> a -> a -> Bool
symmetric_ f = symmetric f f

antisymmetric :: Prelude.Eq a => (a -> a -> Bool) -> a -> a -> Bool
antisymmetric f x y = f x y && f y x ===> (x == y)

leftIdentity :: Prelude.Eq b => (a -> b -> b) -> a -> b -> Bool
leftIdentity f e b = f e b == b

rightIdentity :: Prelude.Eq a => (a -> b -> a) -> b -> a -> Bool
rightIdentity f e a = f a e == a

identity :: Prelude.Eq a => (a -> a -> a) -> a -> a -> Bool
identity f e x = leftIdentity f e x && rightIdentity f e x

associative :: Prelude.Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = lhs == rhs
    where
        lhs = (x `f` y) `f` z
        rhs = x `f` (y `f` z)

commutative :: Prelude.Eq c => (a -> b -> c) -> (b -> a -> c) -> a -> b -> Bool
commutative f g a b = (f a b) == (g b a)

commutative_ :: Prelude.Eq a => (a -> a -> a) -> a -> a -> Bool
commutative_ f x y = (f x y) == (f y x)

inverse :: Prelude.Eq a => (a -> b) -> (b -> a) -> a -> Bool
inverse f g x = g (f x) == x

dual :: (Prelude.Eq a, Prelude.Eq b) => (a -> b) -> (b -> a) -> a -> b -> Bool
dual f g a b = inverse f g a && inverse g f b

involution :: Prelude.Eq a => (a -> a) -> a -> Bool
involution f x = f (f x) == x
