module Alpha.Data.Bag
(
    Bag, countDistinct
)
where

import Alpha.Base
import Alpha.Canonical
import qualified Data.MultiSet as MS

type Bag a = MS.MultiSet a

-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = MS.fromList

countDistinct::Bag a -> Int
countDistinct = MS.distinctSize

instance (Ord a, PartialOrder a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = MS.toList
    fromList = bag

instance Counted (Bag a) where
    count = fromIntegral . MS.size

instance (Ord a, PartialOrder a) => Container (Bag a) where        
    contain = MS.fromList
    contents = MS.toList

instance (Ord a, PartialOrder a) => Setwise (Bag a) where
    intersect = MS.intersection
    delta = MS.difference
    union = MS.union
    