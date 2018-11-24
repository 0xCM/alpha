module Alpha.Data.Poset
(
    Poset, poset
)
where
import Alpha.Base
import Alpha.Canonical
import qualified Data.Set as S

-- Encloses (constructively) a partially ordered set
newtype Poset a = Poset (Set a)
    deriving(Show)

-- Constructs a partially ordered set from a list
poset::(Ord a, PartialOrder a) => [a] -> Poset a
poset = Poset . S.fromList

instance (Eq a, Ord a, PartialOrder a) => Container (Poset a) a where
    singleton x = poset [x]    

instance (Eq a, Ord a, PartialOrder a) => Setwise (Poset a) a where
    union (Poset s1) (Poset s2) = Poset $ S.union s1 s2 
    intersect (Poset s1) (Poset s2) = Poset $ S.intersection s1 s2
    delta (Poset s1) (Poset s2) =  Poset $ S.difference s1 s2
