-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Setwise
(
    Setwise(..),

)
where

import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations
import Alpha.Canonical.Collective.Containers

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag

-- | Characterizes types whose values can be treated as sets
class (Container c) => Setwise c where
    -- The union operator
    union::BinaryOperator c
    -- The intersection operator
    intersect::BinaryOperator c
    -- The set difference operator
    delta::BinaryOperator c
    
-- | Characterizes a finite set
class (Setwise c, FiniteContainer c) => FiniteSet c where    

instance (Ord a) => FiniteSet (Set a) where

instance (Eq a) => Setwise [a] where
    union = List.union
    intersect = List.intersect
    delta =  (List.\\)

instance (Ord a) => Setwise (Set a) where
    union = Set.union
    intersect = Set.intersection
    delta  = Set.difference 
    
instance (Ord a, PartialOrder a) => Setwise (Bag a) where
    intersect = Bag.intersection
    delta = Bag.difference
    union = Bag.union        
