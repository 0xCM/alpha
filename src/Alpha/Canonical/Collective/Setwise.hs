-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Setwise where

import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Collective.ItemSet
import Alpha.Canonical.Collective.Containers

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag


instance (Eq a, Ord a) => Setwise [a] where
    union = List.union
    intersect = List.intersect
    delta =  (List.\\)
    subset proper candidate source  
        = subset proper  (ItemSet $ Set.fromList candidate)  (ItemSet $ Set.fromList source)

    
instance (Ord a) => Setwise (Bag a) where
    intersect = Bag.intersection
    delta = Bag.difference
    union = Bag.union        
    subset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)

