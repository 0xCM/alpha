{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Algebra.VectorSpace
(
    VectorSpace(..),    
    Normed(..),    
    InnerProductSpace(..)

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Field
import Alpha.Canonical.Algebra.Module

class (Field k, LeftModule k v) => VectorSpace k v where

class (KnownNat n, VectorSpace k v) => VectorSpaceN n k v where
    dim::(Integral i) => i
    dim = natg @n

class (VectorSpace k v) => InnerProductSpace k v where
    dot::v -> v -> k

    (.*.)::v -> v -> k
    (.*.) = dot


class Normed a where
    type Norm a
    norm::a -> Norm a

