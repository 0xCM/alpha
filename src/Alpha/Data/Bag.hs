module Alpha.Data.Bag
(
    Bag, countDistinct
)
where

import Alpha.Base
import Alpha.Canonical
import qualified Data.MultiSet as MS

type Bag = MS.MultiSet

-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = MS.fromList

countDistinct::Bag a -> Int
countDistinct = MS.distinctSize

instance Counted (Bag a) where
    count = fromIntegral . MS.size

instance (Ord a) => Container (Bag a) a where        
    singleton = MS.singleton

instance (Ord a) => Setwise (Bag a) a where
    intersect = MS.intersection
    delta = MS.difference
    union = MS.union
    