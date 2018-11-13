module Alpha.Canonical.Relations
(    
    Relation(..),
    TotalOrder(..), 
    Equivalence(..)

) where

import Alpha.Base
import Alpha.Canonical.Operators
import qualified Prelude as P

-- Encodes that values a and by are related via a relation r
data Related r a b = Related r (a,b)
    deriving (Show,Ord,Eq)

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

-- Defines a relation between a and b via r
related::r -> (a,b) -> Related r a b
related  = Related
