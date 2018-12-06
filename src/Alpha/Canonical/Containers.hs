-----------------------------------------------------------------------------
-- | Fundamental container-related constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Containers
(
    Container(..),
    FiniteContainer(..),
    FiniteSet(..),
    Filterable(..),
    Sequential(..),
    Setwise(..),
    Listing(..),
    Unlisted(..)
)
where

import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import Alpha.Canonical.Numeric

import qualified Data.List as L

class Listing a where
    type ListItem a
    list::a -> [ListItem a]

class (Listing a) => Unlisted a  where
    unlist::[ListItem a] -> a

-- / Characterizes a container 'c' dispensing elements of type e
class (IsList c) => Container c where
    -- | Constructs a container from a list of elements, equivalent to 'fromList'
    contain::[Item c] -> c
    contain = fromList

    -- Produces the contained elements, equivalent to 'toList'
    contents::c -> [Item c]
    contents = toList

    -- | Constructs a container with exactly one element
    singleton::Item c -> c
    singleton e = contain [e]


-- | Characterizes types whose values can be treated as sets
class (Container c) => Setwise c where
    -- The union operator
    union::BinaryOperator c
    -- The intersection operator
    intersect::BinaryOperator c
    -- The set difference operator
    delta::BinaryOperator c

-- | Characterizes a container that holds a finite number of elements    
class (Counted c, Container c) => FiniteContainer c where
    -- Produces the contained elements
    --contents::c -> [e]

-- | Characterizes a finite set
class (Setwise c, FiniteContainer c) => FiniteSet c where    
    
-- | Characterizes a container holding elements that can be 
-- filtered via a unary predicate    
class (Container c) => Filterable c where

    -- | Excludes elements that don't satisfy a predicate
    filter::UnaryPredicate (Item c) -> c -> c

    -- | Selects a single element that satisfies a predicate
    single::UnaryPredicate (Item c)-> c -> Item c
    single p c =  L.head $ contents $ filter p c  

class (Container c ) => Sequential c  where 
    
    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> c -> Seq (Item c)
    
    -- | Skips the first item of the sequence and returns the remainder
    tail::c -> c

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::UnaryPredicate (Item c) -> c -> (c, c)

    -- | Returns elements until a supplied predicate is disatisfied
    while::UnaryPredicate (Item c) -> c -> c

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> c -> c

-- Produces a list for types that support the 'IsList' constraint    
list'::(IsList a) => a -> [Item a]
list' = toList