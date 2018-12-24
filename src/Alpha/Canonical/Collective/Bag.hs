-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Bag
(
    bag    
) where
import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Container
import qualified Data.MultiSet as Bag
import qualified Data.List as List

type instance Individual (Bag a) = a



-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = Bag.fromList

countDistinct::Bag a -> Int
countDistinct = Bag.distinctSize

instance (Ord a) => Container (Bag a)
    
instance Vacant (Bag a) where
    empty = Bag.empty
    null = Bag.null

instance Finite (Bag e) where
    count = fromIntegral . Bag.size
    
instance (Ord a) => Setwise (Bag a) where
    intersect = Bag.intersection
    delta = Bag.difference
    union = Bag.union        
    isSubset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)
    
instance (Ord a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag
            