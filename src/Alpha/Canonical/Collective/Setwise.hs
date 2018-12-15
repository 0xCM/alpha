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
    -- The set membership test operator
    subset::Bool -> c -> c -> Bool
    
instance (Eq a, Ord a) => Setwise [a] where
    union = List.union
    intersect = List.intersect
    delta =  (List.\\)
    subset proper candidate source  
        = subset proper (Set.fromList candidate)  (Set.fromList source)

instance (Ord a) => Setwise (ItemSet a) where
    union = Set.union
    intersect = Set.intersection
    delta  = Set.difference 
    subset proper candidate source 
        = ifelse proper 
            (Set.isProperSubsetOf candidate source) 
            (Set.isSubsetOf candidate source)
    
instance (Ord a) => Setwise (Bag a) where
    intersect = Bag.intersection
    delta = Bag.difference
    union = Bag.union        
    subset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)

