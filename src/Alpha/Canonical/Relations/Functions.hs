-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Functions
(
    PairFunc(..), Pairs(..),
    Func, UnaryFunc, CartesianFunc, BinaryFunc, TernaryFunc, Functional(..),
    Compositional(..), Composition(..)

) where
import Alpha.Base
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

-- A synonym for the canonical unary function type
type Func a b = a -> b

-- | Defines a collection of ordered pairs    
newtype Pairs a b = Pairs [(a,b)]
    deriving (Show,Eq,Ord,Generic)
instance Newtype (Pairs a b)

-- | Defines a collection of ordered pairs, no two of which have the
-- same first term and is thus, by definition, a function    
newtype PairFunc a b = PairFunc (Map a b)
    deriving (Show,Eq,Ord,Generic)
instance Newtype (PairFunc a b)

-- | Characterizes types from which functions may be extracted
class Functional (f::Type->Type->Type) (a::Type) (b::Type) where    
    func::f a b -> Func a b
    
    
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


instance (Eq a, Eq b) => Functional Pairs a b where
    func (Pairs pairs) =  \a -> Maybe.fromJust $ List.lookup a pairs

instance Ord a => Functional PairFunc a b where    
    func (PairFunc m) =  (Map.!) m

class Compositional g f where
    compose::g -> f -> Composition g f

type family Composition g f
type instance Composition (Func b c) (Func a b) = Func a c
type instance Composition (PairFunc b c) (PairFunc a b) = PairFunc a c
        
instance Compositional (Func b c) (Func a b) where
    compose g f = g . f    
    
instance (Ord a, Ord b) => Compositional (PairFunc b c) (PairFunc a b) where
    compose mg mf = mh where
        f = func mf
        g = func mg
        h = g . f
        z = (\a -> (a, h a)) <$> Map.keys (unwrap mf)
        mh = PairFunc $ Map.fromList z

newtype FComposition f g a = FComposition ( f (g a) )

instance (Functor f, Functor g) => Functor (FComposition f g) where
    fmap f (FComposition x) = FComposition (fmap (fmap f) x )