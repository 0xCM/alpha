-----------------------------------------------------------------------------
-- | Defines curry and uncurry operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Curry
(
    Uncurried, Uncurriable,
    Curried, Curriable
) where

import Alpha.Base
import Alpha.Canonical.Operators


-- | Represents an injective family of type-level functions that produce
-- uncurried types
type family Uncurried a = r | r -> a
type instance Uncurried (BinaryFunc a b c) = CartesianFunc a b c

-- | Represents an injective family of type-level functions that produce
-- curried types
type family Curried a = r | r -> a
type instance Curried (CartesianFunc a b c) = BinaryFunc a b c

-- | Characterizes a curried function vis-a-vis:
-- A function f is **curribale** if there exists a transformation
-- curry:f -> Curried f that carries a function to a valid 'Curried'
-- form
class Curriable f where
    curry::f -> Curried f

-- | Characterizes an uncurried function vis-a-vis:
-- A function f is **uncurribale** if there exists a transformation
-- uncurry:f -> 'Uncurried' f that carries a function to a valid 'Uncurried'
-- form
class Uncurriable f where
    uncurry::f -> Uncurried f

-- | Canonical curry function, for purposes of illustration
curry':: ((a,b) -> c) -> (a -> b -> c)
curry' f x y =  f (x, y)

-- | Canonical uncurry function, for purposes of illustration
uncurry'::(a -> b -> c) -> ((a,b) -> c)
uncurry' f p = f (fst p) (snd p)
    
instance Curriable (CartesianFunc a b c) where
    curry::CartesianFunc a b c -> Curried (CartesianFunc a b c)
    curry f x y =  f (x, y)

instance Uncurriable (BinaryFunc a b c) where
    uncurry::BinaryFunc a b c -> Uncurried (BinaryFunc a b c)
    uncurry f p = f (fst p) (snd p)

