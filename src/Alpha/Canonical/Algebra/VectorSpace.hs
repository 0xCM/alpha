{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Algebra.VectorSpace
(
    VectorSpace(..),
    Dimension(..), 
    Normed(..),    

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Field
import Alpha.Canonical.Algebra.Module

class (Field k, LeftModule k v) => VectorSpace k v where


class Normed a where
    type Norm a
    norm::a -> Norm a

