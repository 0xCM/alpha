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

instance (Ord a) => Unionizable (Bag a) where
    union = MS.union

instance (Ord a) => Intersectable (Bag a) where
    intersect = MS.intersection

instance (Ord a) => Diffable (Bag a) where
    delta = MS.difference

instance (Ord a) => Container (Bag a) a where    
    type Source (Bag a) a = Bag a
    singleton = MS.singleton