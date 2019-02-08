-----------------------------------------------------------------------------
-- | 
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
    Img(..),
    End(..),
    Func(..),
    Preimage(..),
    Composition(..), 
    Compositional(..), 
    Functional(..),
    Uncurried, Curried, 
    Curriable(..), Uncurriable(..),
) where
import Alpha.Canonical.Relations.Common

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

newtype Func0 a = Func0 (F0 a)
    deriving (Generic)
instance Newtype (Func0 a)

newtype Func1 a1 a2 = Func1 (F1 a1 a2) 
    deriving (Generic)
instance Newtype (Func1 a1 a2)

newtype Func2 a1 a2 a3 = Func2 (F2 a1 a2 a3)
    deriving (Generic)
instance Newtype (Func2 a1 a2 a3)

newtype Func3 a1 a2 a3 a4 = Func3 (F3 a1 a2 a3 a4)
    deriving (Generic)
instance Newtype (Func3 a1 a2 a3 a4)

newtype Func4 a1 a2 a3 a4 a5 = Func4 (F4 a1 a2 a3 a4 a5)
    deriving (Generic)
instance Newtype (Func4 a1 a2 a3 a4 a5)

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
type instance Dom (Func0 a) = a
type instance Dom (Func1 a1 a2) = a1
type instance Dom (Func2 a1 a2 a3) = (a1,a2)
type instance Dom (Func3 a1 a2 a3 a4) = (a1,a2,a3)
type instance Dom (Func4 a1 a2 a3 a4 a5) = (a1,a2,a3,a4)

-- Defines a family of types that specify function *codomains*,\
-- i. e. identified sets that cover, but are not necessarily equal
-- to, thes function images
type family Cod f     
type instance Cod (a -> b) = b
type instance Cod (Map a b) = b
type instance Cod (Func0 a) = a
type instance Cod (Func1 a1 a2) = a2
type instance Cod (Func2 a1 a2 a3) = a3
type instance Cod (Func3 a1 a2 a3 a4) = a4
type instance Cod (Func4 a1 a2 a3 a4 a5) = a5


-- | The image is a subset of the codomain and can be typed accordingly
type Img f  = Cod f


type family Func f      
type instance Func (a -> b) = a -> b
type instance Func (Map a b)  = Map a b
type instance Func (Func0 a) = a
type instance Func (Func1 a1 a2) = a1 -> a2
type instance Func (Func2 a1 a2 a3) = (a1,a2) -> a3
type instance Func (Func3 a1 a2 a3 a4) = (a1,a2,a3) -> a4
type instance Func (Func4 a1 a2 a3 a4 a5) = (a1,a2,a3,a4) -> a5

-- Encodes a bijective function
newtype Bijection a b = Bijection (a -> b, b -> a)

-- | Characterizes a function for which preimages of codomain sets
-- can be calculated
class Preimage f where
    preimage::f -> Cod f -> Dom f

-- | Characterizes a structure-preserving endomorphism
class Category c => End c where
    end::c a b -> (a -> b)

type Hom f a b = (Function f, a ~ Dom f, b ~ Cod f)
    
class Function f where
    fx::f -> Func f

instance Function (a -> b) where
    fx::(a -> b) -> a -> b
    fx = id


-- | Characterizes function composition    
class Compositional g f where
    compositon::g -> f -> Composition g f
    

class Functional f where    
    func::Func f -> ((Dom f) -> (Cod f))
    
instance Functional (Func0 a1 ) where
    func a = id
    
instance Functional (Func1 a1 a2) where
    func f = f
    
instance Functional (Func2 a1 a2 a3) where
    func f = f

instance Functional (Func3 a1 a2 a3 a4) where
    func f = f

instance Functional (Func4 a1 a2 a3 a4 a5) where
    func f = f        

instance forall f a b. (Ord a, a ~ Dom f, b ~ Cod f, f ~ Map a b) => Functional (Map a b) where    
    func m = g
        where
            g::Dom f -> Cod f
            g x = m Map.! x

instance forall f a b. (a ~ Dom f, b ~ Cod f, f ~ (a -> b)) => Functional (a -> b) where    
    func f = f
                        
instance (Ord a, Ord b) => Compositional (Map b c) (Map a b) where
    compositon mg mf= mh where
        f = func' mf
        g = func' mg
        h = g . f
        z = (\a -> (a, h a)) <$> Map.keys mf
        mh = Map.fromList z
        func' m =  (Map.!) m
    
instance Category Func1 where
    id = Func1(\x -> x)
    g . f = wrap $ (unwrap g) . (unwrap f)

instance Curriable (CartesianFunc a b c) where
    curry::CartesianFunc a b c -> Curried (CartesianFunc a b c)
    curry f x y =  f (x, y)

instance Uncurriable (a -> b -> c) where
    uncurry::(a -> b -> c) -> Uncurried (a -> b -> c)
    uncurry f p = f (fst p) (snd p)
    
