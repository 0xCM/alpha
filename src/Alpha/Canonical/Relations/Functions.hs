-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations.Functions
(
    Dom(..),
    Cod(..),
    Arity(..),
    Composition(..), 
    Compositional(..), 
    Function(..),
    Functional(..),
    Uncurried, Curried, 
    Curriable(..), Uncurriable(..),
    Computable(..),    
) where
import Alpha.Base

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

newtype F0 a = F0 a
    deriving (Generic)
instance Newtype (F0 a)

newtype F1 a1 a2 = F1 (a1 -> a2)
    deriving (Generic)
instance Newtype (F1 a1 a2)

-- | Function representative that saturates with 2 heterogenous arguments
-- that aligns with the follwing definition
-- A **binary function** is a function that accepts two arguments and
-- produces a value. Note that a binary function cannot be cartesian
-- nor conversely.
newtype F2 a1 a2 a3 = F2 (a1 -> a2 -> a3)
    deriving (Generic)
instance Newtype (F2 a1 a2 a3)

newtype F3 a1 a2 a3 a4 = F3 (a1 -> a2 -> a3 -> a4)
newtype F4 a1 a2 a3 a4 a5 = F4 (a1 -> a2 -> a3 -> a4 -> a5)

type family Func (n::Nat) a = r | r -> n where
    Func 0 a = F0 a
    Func 1 (a1,a2) = F1 a1 a2
    Func 2 (a1,a2,a3) = F2 a1 a2 a3
    Func 3 (a1,a2,a3,a4) = F3 a1 a2 a3 a4
    Func 4 (a1,a2,a3,a4,a5) = F4 a1 a2 a3 a4 a5

-- | Synonym for function that saturates with 1 cartesian argument
-- that aligns with the following definiion:
-- A **Cartesian function** is any function whose domain consists
-- of 2-tuples, i.e., f:(a,b) -> c 
type CartesianFunc a b c = (a,b) -> c

-- | Represents an injective family of type-level functions that produce
-- uncurried types
type family Uncurried a = r | r -> a
type instance Uncurried (a -> b -> c) = CartesianFunc a b c

-- | Represents an injective family of type-level functions that produce
-- curried types
type family Curried a = r | r -> a
type instance Curried (CartesianFunc a b c) = a -> b -> c

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

-- | Defines a collection of ordered pairs, no two of which have the
-- same first term and is thus, by definition, a function    
newtype PairFunc a b = PairFunc (Map a b)
    deriving (Generic,Eq,Ord,Show)
instance Newtype (PairFunc a b)


-- | Defines a family of types that specify heterogenous function composition
-- More precisely, given a function g whose domain coincides with
-- the codomain of a function f the 'Composition' type of g and f
-- is the type of the function h:Dom f -> Cod g where h = g . f
type family Composition g f
type instance Composition (b -> c) (a -> b) = a -> c
type instance Composition (Map b c) (Map a b) = Map a c

-- Defines a family of types that specify function domains
type family Dom f 
type instance Dom (a -> b) = a
type instance Dom (Map a b) = a
type instance Dom (F0 a) = a
type instance Dom (F1 a1 a2) = a1
type instance Dom (F2 a1 a2 a3) = (a1,a2)
type instance Dom (F3 a1 a2 a3 a4) = (a1,a2,a3)
type instance Dom (F4 a1 a2 a3 a4 a5) = (a1,a2,a3,a4)

-- Defines a family of types that specify function codomains
type family Cod f     
type instance Cod (a -> b) = b
type instance Cod (Map a b) = b
type instance Cod (F0 a) = a
type instance Cod (F1 a1 a2) = a2
type instance Cod (F2 a1 a2 a3) = a3
type instance Cod (F3 a1 a2 a3 a4) = a4
type instance Cod (F4 a1 a2 a3 a4 a5) = a5


type family Function f      
type instance Function (a -> b) = a -> b
type instance Function (Map a b)  = Map a b
type instance Function (F0 a) = a
type instance Function (F1 a1 a2) = a1 -> a2
type instance Function (F2 a1 a2 a3) = (a1,a2) -> a3
type instance Function (F3 a1 a2 a3 a4) = (a1,a2,a3) -> a4
type instance Function (F4 a1 a2 a3 a4 a5) = (a1,a2,a3,a4) -> a5

type family Arity f = (r::Nat) 
type instance Arity (F0 a) = 0
type instance Arity (F1 a1 a2) = 1
type instance Arity (F2 a1 a2 a3) = 2
type instance Arity (F3 a1 a2 a3 a4) = 3
type instance Arity (F4 a1 a2 a3 a4 a5) = 4

-- | Characterizes a deferred computation or a computation
-- specification    
class Computable a where
    -- | The type of computed value
    type Computed a

    -- | Effects the computation
    compute::a -> Computed a

-- | Characterizes function composition    
class Compositional g f where
    compose::g -> f -> Composition g f
    

class Functional f where    
    func::Function f -> ((Dom f) -> (Cod f))

    

instance Functional (F0 a1 ) where
    func a = id
    
instance Functional (F1 a1 a2) where
    func f = f
    
instance Functional (F2 a1 a2 a3) where
    func f = f

instance Functional (F3 a1 a2 a3 a4) where
    func f = f

instance Functional (F4 a1 a2 a3 a4 a5) where
    func f = f        

instance forall f a b. (Ord a, a ~ Dom f, b ~ Cod f, f ~ Map a b) => Functional (Map a b) where    
    func m = g
        where
            g::Dom f -> Cod f
            g x = m Map.! x

instance forall f a b. (a ~ Dom f, b ~ Cod f, f ~ (a -> b)) => Functional (a -> b) where    
    func f = f
                        
instance (Ord a, Ord b) => Compositional (Map b c) (Map a b) where
    compose mg mf= mh where
        f = func' mf
        g = func' mg
        h = g . f
        z = (\a -> (a, h a)) <$> Map.keys mf
        mh = Map.fromList z
        func' m =  (Map.!) m
    
instance Category F1 where
    id = F1(\x -> x)
    g . f = wrap $ (unwrap g) . (unwrap f)

instance Curriable (CartesianFunc a b c) where
    curry::CartesianFunc a b c -> Curried (CartesianFunc a b c)
    curry f x y =  f (x, y)

instance Uncurriable (a -> b -> c) where
    uncurry::(a -> b -> c) -> Uncurried (a -> b -> c)
    uncurry f p = f (fst p) (snd p)
    