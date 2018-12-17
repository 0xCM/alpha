{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Relations.Undecidable where
import Alpha.Base

import Alpha.Canonical.Relations.Related

-- | Encodes that values of orderable types can be related via (<=)
instance (Ord a) => LTEQ a

-- | Encodes that values of orderable types can be related via (<)
instance (Ord a) => LT a    

-- | Encodes that values of orderable types can be related via (>)
instance (Ord a) => GT a    

-- | Encodes that values of orderable types can be related via (>=)
instance (Ord a) => GTEQ a    

-- | Encodes that values of orderable types are uniformly comparable
instance (Ord a) => Comparable a

-- | Encodes that reflexive and tansitive relations are, by definition, preorders
instance (Reflexive a, Transitive a) => Preorder a

-- | Encodes that reflexive and tansitive relations are, by definition, equivalence relations
instance (Reflexive a, Symmetric a, Transitive a) => Equivalence a

