-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Functions.Common
(    
    Func, UnaryFunc, CartesianFunc, BinaryFunc, TernaryFunc,    
    Uncurried, Curried, 
    Curriable(..), Uncurriable(..),
    Computable(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Elementary

-- A synonym for the canonical unary function type
type Func a b = a -> b

-- | Synonym for function that saturates with 1 argument
type UnaryFunc a b = Func a b

-- | Synonym for endomorphism
type EndoFunc a = Func a a

-- | Synonym for function that saturates with 1 cartesian argument
-- that aligns with the following definiion:
-- A **Cartesian function** is any function whose domain consists
-- of 2-tuples, i.e., f:(a,b) -> c 
type CartesianFunc a b c = Func (a,b) c

-- | Synonym for function that saturates with 2 heterogenous arguments
-- that aligns with the follwing definition
-- A **binary function** is a function that accepts two arguments and
-- produces a value. Note that a binary function cannot be cartesian
-- nor conversely.
type BinaryFunc a b c = a -> b -> c

-- | Function synonym that saturates with 3 heterogenous arguments
type TernaryFunc a b c d = a -> b -> c -> d

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


-- | Characterizes a deferred computation or a computation
-- specification    
class Computable a where
    -- | The type of computed value
    type Computed a

    -- | Effects the computation
    compute::a -> Computed a
