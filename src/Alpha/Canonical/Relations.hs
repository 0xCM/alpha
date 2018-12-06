{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations
(    
    Relation(..),
    Equivalence(..),
    PartialOrd(..),
    PartialOrder(..),
    TotalOrder(..),
    Pairing(..),
    Infimum(..),
    Supremum(..),
    Spanned(..), 
    IntegralSpan(..),
    Degenerate(..),    
    JoinSemiLattice(..), 
    MeetSemiLattice(..), 
    Lattice(..),

    (<=), (<), (>=), (>),
    min, max, swap
) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Operators
import Algebra.Lattice(JoinSemiLattice((\/)),MeetSemiLattice((/\)))
import Algebra.Lattice(Lattice(..))   
import qualified Data.List as List
import qualified Prelude as P

-- Characterizes a relation on a set s    
class Relation a where

    -- Relation adjudicator
    relator::BinaryPredicate a

    -- Infix synonym for 'relator'
    (~~)::BinaryPredicate a
    (~~) = relator

infixl 6 ~~

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

type TotalOrder a = Ord a

-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimum a b where
    -- / The greatest lower bound
    infimum::a -> b
    
-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremum a b where
    -- / The least upper bound
    supremum::a -> b

-- | Characterizes a type that contains a relatively contiguous
-- set of values bound by least and greatest values
class (Ord b, Supremum a b, Infimum a b) => Spanned a b where
    
    -- | Creates a b-value bound by a min and max value
    span::b -> b -> a
    
    -- | The span operator, an infix synonym for 'span'
    (...)::b -> b -> a
    (...) = span

infixl 5 ...

newtype IntegralSpan a = IntegralSpan [a]

-- Characterizes types that are inhabited by 'degenerate' values
-- Examples include empty lists, mathematical intervals 
-- that represent a single value, etc. What precicely constitutes a 
-- a degenerate value for a given type is implementation-defined
-- See https://en.wikipedia.org/wiki/Degeneracy_(mathematics)
class Degenerate a where
    -- Test for degeneracy
    degenerate::a -> Bool     

-- | Represents an ordered relationshp between two elements
class Pairing a b where
    -- Specifies the type of a pair
    type Paired a b    

    -- | Pairs two elements
    pair::a -> b -> Paired a b

    --- | Extracts the first of the paired elements
    first::Paired a b -> a
    
    --- | Extracts the second of the paired elements
    second::Paired a b -> b

-- | The canonical/obligatory swap function    
swap::(Pairing a b, Pairing b a) => Paired a b -> Paired b a
swap x = pair (second x) (first x)

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

max::(Ord a) => a -> a -> a
max x y = ifelse (x >= y) x y
    
instance (Ord a, Integral a) => Infimum (IntegralSpan a) a where
    infimum (IntegralSpan s) = List.head s

instance (Ord a, Integral a) => Supremum (IntegralSpan a) a where
    supremum (IntegralSpan s) = List.last s
        
instance (Ord a, Integral a) => Spanned (IntegralSpan a) a where
    span min max = IntegralSpan [min .. max]
    
instance Pairing a b where
    type Paired a b = (a,b)    
    first (a,b) = a
    second (a,b) = b
    pair a b = (a,b)
