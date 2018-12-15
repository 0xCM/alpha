module Alpha.Canonical.Algebra.DisjointUnion
(
    DisjointUnion(..),
    Disjunctive(..)    

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Pairing

-- | Represents a dijoint union of elements
-- See https://en.wikipedia.org/wiki/Disjoint_union
newtype DisjointUnion a b = DisjointUnion (a, b)

type instance Paired a b (DisjointUnion a b) = (DisjointUnion a b)

instance Pairing (ItemSet a) (ItemSet b) (DisjointUnion (ItemSet a) (ItemSet b)) where
    pair a b = DisjointUnion (a, b)
    first (DisjointUnion (a,b)) = a
    second (DisjointUnion (a,b)) = b
    
class Disjunctive a b where
    disjoint::a -> b -> DisjointUnion a b
    disjoint a b = DisjointUnion (a,b)