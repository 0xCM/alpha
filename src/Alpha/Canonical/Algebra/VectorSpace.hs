{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Algebra.VectorSpace
(
    VectorSpace(..),
    Dimension(..), 
    Dimensional(..),        
    Normed(..),    

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Field
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Action
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Ring

class (Field k, Additive v, LeftAction k v) => VectorSpace k v where


class Normed a where
    type Norm a
    norm::a -> Norm a

