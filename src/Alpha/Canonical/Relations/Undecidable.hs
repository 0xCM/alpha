{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Relations.Undecidable where
import Alpha.Base
import Alpha.Canonical.Relations.Related

import qualified Prelude as P

instance (Eq a, Ord a, Num a) => PartialOrd a where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance (Ord a, PartialOrd a) => PartialOrder a where
    (<=) a b = a P.<= b
    {-# INLINE (<=) #-}

-- | Encodes that values of orderable types can be related via (<=)
instance (Ord a,PartialOrder a) => LTEQ a

-- | Encodes that values of orderable types can be related via (<)
instance (Ord a) => LT a    

-- | Encodes that values of orderable types can be related via (>)
instance (Ord a) => GT a    

-- | Encodes that values of orderable types can be related via (>=)
instance (Ord a) => GTEQ a    

-- | Encodes that values of orderable types are uniformly comparable
instance (Ord a, PartialOrd a) => Comparable a

-- | Encodes that reflexive and tansitive relations are, by definition, preorders
instance (Reflexive a, Transitive a) => Preorder a

-- | Encodes that reflexive and tansitive relations are, by definition, equivalence relations
instance (Reflexive a, Symmetric a, Transitive a) => Equivalence a

