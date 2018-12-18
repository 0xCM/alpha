module Alpha.Canonical.Relations.DisjointUnion
(
    DisjointUnion(..),
    disjoint

) where
import Alpha.Base
import Alpha.Canonical.Relations.Pairing

-- | Represents a dijoint union of elements
-- See https://en.wikipedia.org/wiki/Disjoint_union
newtype DisjointUnion a b = DisjointUnion (a, b)

type instance Paired a b (DisjointUnion a b) = (DisjointUnion a b)

-- | Constructs a disjoint union
disjoint::a -> b -> DisjointUnion a b
disjoint a b = DisjointUnion (a,b)

    
