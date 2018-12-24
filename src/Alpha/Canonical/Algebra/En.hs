-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Algebra.En
(
    En(..),
)
where 
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Action
import Alpha.Canonical.Algebra.Ring
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Module
import Alpha.Canonical.Algebra.Group

-- | Represents Euclidean space for some n >= 1
data family En (n::Nat) k 
data instance En 1 k = E1 (UniTuple 1 k)
data instance En 2 k = E2 (UniTuple 2 k) 
data instance En 3 k = E3 (UniTuple 3 k)
data instance En 4 k = E4 (UniTuple 4 k) 
data instance En 5 k = E5 (UniTuple 5 k) 

vecN::(KnownNat n) => UniTuple n k -> En n k
vecN = undefined

instance Formattable k => Formattable (En 1 k) where
    format (E1 x) = format x
instance Formattable k => Formattable (En 2 k) where
    format (E2 x) = format x
instance Formattable k => Formattable (En 3 k) where
    format (E3 x) = format x
instance Formattable k => Formattable (En 4 k) where
    format (E4 x) = format x
instance Formattable k => Formattable (En 5 k) where
    format (E5 x) = format x
                    
instance Formattable (En n k) => Show (En n k) where
    show  = string . format

instance Additive k => Additive (En 1 k) where
    add (E1 x) (E1 y) = E1 (add x y)
instance Additive k => Additive (En 2 k) where
    add (E2 x) (E2 y) = E2 (add x y)
instance Additive k => Additive (En 3 k) where
    add (E3 x) (E3 y) = E3 (add x y)
instance Additive k => Additive (En 4 k) where
    add (E4 x) (E4 y) = E4 (add x y)
instance Additive k => Additive (En 5 k) where
    add (E5 x) (E5 y) = E5 (add x y)

instance (Multiplicative k) =>  LeftAction k (En 1 k) where
    type LeftProduct k (En 1 k) = k
    k *. (E1 (UniTuple1 x) ) = k * x
instance (Multiplicative k) =>  LeftAction k (En 2 k) where
    k *. (E2 (x1,x2)) = E2  (k*x1,k*x2)
instance (Multiplicative k) =>  LeftAction k (En 3 k) where
    k *.  (E3 (x1,x2,x3)) = E3 (k*x1,k*x2,k*x3)    
instance (Multiplicative k) =>  LeftAction k (En 4 k) where
    k *.  (E4 (x1,x2,x3,x4)) = E4 (k*x1,k*x2,k*x3,k*x4)
instance (Multiplicative k) =>  LeftAction k (En 5 k) where
    k *.  (E5 (x1,x2,x3,x4,x5)) = E5 (k*x1,k*x2,k*x3,k*x4,k*x5)
                        
kdelta::forall i j k. (KnownNat i, Integral j, Integral k) => j -> k
kdelta j = ifelse (i == j) 1 0 where i = natg @i
    
-- | Euclidean n-space        
instance (v ~ En n k, KnownNat n, AbelianGroup v,  Ring k, LeftAction k v) => LeftModule k v                