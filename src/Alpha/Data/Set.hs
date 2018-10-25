module Alpha.Data.Set
(
    Set
)
where
import Data.HashSet as Set
import Data.Hashable
import Data.Eq
import Alpha.Canonical


type Set a = HashSet a

instance Enumerable (Set a) a where
    type Source (Set a) a = Set a
    items = toList

instance (Hashable a) => Singletary (Set a) a where
    singleton = Set.singleton
    
instance (Eq a, Hashable a) => Existential (Set a) a where
    exists = Set.member