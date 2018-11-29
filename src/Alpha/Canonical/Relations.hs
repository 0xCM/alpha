module Alpha.Canonical.Relations
(    
    Relation(..),
    Equivalence(..),
    PartialOrd(..),
    PartialOrder(..),
    TotalOrder(..),
    Pairing(..),
    (<=), (<), (>=), (>),
    min, max, swap
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

type TotalOrder a = Ord a

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

instance Pairing a b where
    type Paired a b = (a,b)    
    first (a,b) = a
    second (a,b) = b
    pair a b = (a,b)

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
    