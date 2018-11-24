module Alpha.Canonical.Relations
(    
    Relation(..),
    TotalOrder(..), 
    Equivalence(..),
    PartialOrd(..),
    PartialOrder(..)
) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Operators

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

-- Characterizes a total order relation
-- https://en.wikipedia.org/wiki/Total_order    
class Ord a => TotalOrder a where
    (<=)::BinaryPredicate a
    (<=) a b= a P.<= b

    (<)::BinaryPredicate a
    (<) a b = a P.< b

    (>)::BinaryPredicate a
    (>) a b = a P.> b

    (>=)::BinaryPredicate a
    (>=) a b = a P.>= b

    between::TernaryPredicate a
    between x a b = x >= a || x <= b

infix 4 <=
infix 4 <    
infix 4 >
infix 4 >=    