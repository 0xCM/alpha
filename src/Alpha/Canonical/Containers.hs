-----------------------------------------------------------------------------
-- | Fundamental container-related constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Containers
(
    Container(..),
    Filterable(..),
    Sequential(..),
    Setwise(..)

)
where

import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
    
-- / Characterizes a container 'c' holding elements of type e
class Container c e | c -> e where
    -- | Constructs a container with exactly one element
    singleton::e -> c

-- | Characterizes types whose values can be treated as sets
class (Container c e) => Setwise c e where
    -- The nunion operator
    union::BinaryOperator c
    -- The intersection operator
    intersect::BinaryOperator c
    -- The set difference operator
    delta::BinaryOperator c

class (Container c e) => Filterable c e where
    -- | Excludes elements that don't satisfy a predicate
    filter::UnaryPredicate e ->  c -> c
                    
class (Container c e) => Sequential c e | c -> e where 

    -- | Returns the items as a list
    listed::c -> [e]
    
    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> c -> Seq e
    
    -- | Skips the first item of the sequence and returns the remainder
    tail::c -> c

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::UnaryPredicate e -> c -> (c, c)

    -- | Returns elements until a supplied predicate is disatisfied
    while::UnaryPredicate e -> c -> c

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> c -> c
