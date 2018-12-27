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
import qualified Data.Set as Set

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

instance (Ord a) => Unionizable (Bag a) where    
    union = Bag.union        
    unions bags = (Set.fromList <$> (toList <$> bags)) |> Set.unions |> toList |> bag
        

instance (Ord a) => Intersectable (Bag a) where    
    intersect = Bag.intersection

instance (Ord a) => SetContainment (Bag a) where
    isSubset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)
    
instance (Ord a) => SetDifference (Bag a) where
    diff = Bag.difference
    
instance (Ord a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag
            