-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Poset
(
    Poset, poset
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Collective.Container
import Alpha.Canonical.Collective.ItemSet
import qualified Data.Set as Set

-- Encloses (constructively) a partially ordered set
newtype Poset a = Poset (ItemSet a)
    deriving(Formattable,Setwise)

-- Constructs a partially ordered set from a list
poset::(Ord a, PartialOrder a) => [a] -> Poset a
poset = Poset . fromList

instance (Ord a, PartialOrder a) =>  IsList (Poset a) where
    type Item (Poset a) = a
    toList (Poset s) = toList s    
    fromList = poset

instance (Eq a, Ord a, PartialOrder a) => Container (Poset a) where
    contain x = poset x  
    contents (Poset s) = toList s