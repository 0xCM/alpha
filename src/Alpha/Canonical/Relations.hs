{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations
(    
    Element(..),
    Relation(..),
    Equivalence(..),
    PartialOrd(..),
    PartialOrder(..),
    Setoid(..),    
    JoinSemiLattice(..), 
    MeetSemiLattice(..), 
    Lattice(..),
    Membership(..),
    Counted(..),
    Tupled(..), 
    Tupelizer(..),
    Reifiable(..),
    Cloneable(..),
    Chunkable(..),
    Convertible(..),
    Jailbreak(..),
    minimal, maximal,
    (<=), (<), (>=), (>),
    min, max, 
) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Element
import Algebra.Lattice(JoinSemiLattice((\/)),MeetSemiLattice((/\)))
import Algebra.Lattice(Lattice(..))   
import Data.List.NonEmpty hiding(span)
import qualified Data.List as List
import qualified Prelude as P

-- Represents a family of types that can be represented as tuples
type family Tupled a

-- Characterizes types from which tuples can be constructed    
class Tupelizer a where
    -- | Forms a tuple from the source value
    tuple::a -> Tupled a

-- Characterizes a relation on a set s    
class Relation a where

    -- Relation adjudicator
    relator::BinaryPredicate a

    -- Infix synonym for 'relator'
    (~~)::BinaryPredicate a
    (~~) = relator
    infixl 6 ~~

-- / Characterizes a setoid where the required equivalence relation is 
-- interpreteted as equality
-- See https://en.wikipedia.org/wiki/Setoid
class (Eq a) => Setoid a where
    equals::BinaryPredicate a
    equals x y = x == y

-- Characterizes preorders that are symmetric, and hence 
-- define equivalence relations: a ~= b => b ~= a
class Relation a => Equivalence a where
    -- Equivalence relation adjudicator
    (~=)::BinaryPredicate a
    (~=) = (~~)

class (PartialOrd a, Relation a) =>  PartialOrder a where

    (~<=)::BinaryPredicate a
    (~<=) = leq
    infix 4 ~<=


class Membership s where

    members::s -> Set (Element s) 
        
-- Captures the assertion that values of a type a can be categorized by
-- values of an enumerable type c
class (Enum c) => Classifiable a c where
    classify::(a -> c) -> [a] -> [(c,a)]
    
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n


-- Characterizes a family of singleton types 'a' for which the type's single inhabitant
-- is reifiable
class Reifiable (a::k) r where
    reify::r 
        
-- Characterizes a value 'a' than can be duplicated according to a specification 'b'  
-- yielding a structure 'Cloned a b' that encloses the duplicates
class Cloneable a b where
    type Cloned a b
    
    clone::a -> b -> Cloned a b

-- | Characterizes a finite container or other type that contains elements
-- that can be separated into groups of possibly different sizes
class Chunkable a where
    chunk::Int -> a -> [a]
    
-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a
            
-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    

            
instance Jailbreak Maybe a where
    escape x = fromJust x
        
(<=)::(Ord a) => BinaryPredicate a
(<=) a b= a P.<= b
infix 4 <=
    
(<)::(Ord a) => BinaryPredicate a
(<) a b = a P.< b
infix 4 <    

(>)::(Ord a) => BinaryPredicate a
(>) a b = a P.> b
infix 4 >

(>=)::(Ord a) => BinaryPredicate a
(>=) a b = a P.>= b

between::(Ord a) => TernaryPredicate a
between x a b = x >= a || x <= b
infix 4 >=    

min::(Ord a) => a -> a -> a
min x y = ifelse (x <= y) x y

minimal::(Ord a, Semigroup a) => [a] -> Min a
minimal (x:xs) = Min <$> (x :| xs) |> sconcat

max::(Ord a) => a -> a -> a
max x y = ifelse (x >= y) x y

maximal::(Ord a, Semigroup a) => [a] -> Max a
maximal (x:xs) = Max <$> (x :| xs) |> sconcat

