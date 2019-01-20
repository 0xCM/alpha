-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Alpha.Canonical.Structures.En
(
    En(..),
    NatBasisElement(..),
    NatBasis(..),
    EuclideanVector(..)
)
where 
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.VectorSpace

-- | Represents Euclidean space for  1 <= n <= 5
data family En (n::Nat) k 
data instance En 1 k = E1 (UniTuple 1 k)
data instance En 2 k = E2 (UniTuple 2 k) 
data instance En 3 k = E3 (UniTuple 3 k)
data instance En 4 k = E4 (UniTuple 4 k) 
data instance En 5 k = E5 (UniTuple 5 k) 

--type instance Individual (En n k) = [k]

-- | Characterizes the standard ith basis element in En
class (KnownNat n, KnownNat i, Ring k) =>  NatBasisElement n i k where
    --en::UniTuple n k
    en::En n k

class (KnownNat n, Ring k) => EuclideanVector n k where
    euvector::UniTuple n k -> En n k
                
class (KnownNat n, Ring k) => NatBasis n k where
    natbasis::FiniteBasisSet (En n k)


vecN::(KnownNat n) => UniTuple n k -> En n k
vecN = undefined


type Euclidean n k v = (v ~ En n k, KnownNat n, AbelianGroup v,  Ring k, LeftAction k v)

-- | Defines Euclidean n-space as as k-module with respect
-- to the type v ~ En n k
instance Euclidean n k v => LeftModule k v

instance (Eq k) => Eq (En 1 k) where
    (E1 x) == (E1 y) = x == y
instance (Eq k) => Eq (En 2 k) where
    (E2 x) == (E2 y) = x == y    
instance (Eq k) => Eq (En 3 k) where
    (E3 x) == (E3 y) = x == y    
instance (Eq k) => Eq (En 4 k) where
    (E4 x) == (E4 y) = x == y    
instance (Eq k) => Eq (En 5 k) where
    (E5 x) == (E5 y) = x == y        


instance (Ord k) => Ord (En 1 k) where
    compare (E1 x) (E1 y) = compare x y

instance (Ord k) => Ord (En 2 k) where
    compare (E2 x) (E2 y) = compare x y

instance (Ord k) => Ord (En 3 k) where
    compare (E3 x) (E3 y) = compare x y

instance (Ord k) => Ord (En 4 k) where
    compare (E4 x) (E4 y) = compare x y

instance (Ord k) => Ord (En 5 k) where
    compare (E5 x) (E5 y) = compare x y   
    

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

instance Nullary k => Nullary (En 1 k) where
    zero = E1 zero
instance Nullary k => Nullary (En 2 k) where
    zero = E2 zero
instance Nullary k => Nullary (En 3 k) where
    zero = E3 zero
instance Nullary k => Nullary (En 4 k) where
    zero = E4 zero
instance Nullary k => Nullary (En 5 k) where
    zero = E5 zero
        
instance Subtractive k => Subtractive (En 1 k) where
    sub (E1 x) (E1 y) = E1 (sub x y)
instance Subtractive k => Subtractive (En 2 k) where
    sub (E2 x) (E2 y) = E2 (sub x y)
instance Subtractive k => Subtractive (En 3 k) where
    sub (E3 x) (E3 y) = E3 (sub x y)
instance Subtractive k => Subtractive (En 4 k) where
    sub (E4 x) (E4 y) = E4 (sub x y)
instance Subtractive k => Subtractive (En 5 k) where
    sub (E5 x) (E5 y) = E5 (sub x y)

instance Negatable k => Negatable (En 1 k) where
    negate (E1 x) = E1 (negate x)
instance Negatable k => Negatable (En 2 k) where
    negate (E2 x) = E2 (negate x)
instance Negatable k => Negatable (En 3 k) where
    negate (E3 x) = E3 (negate x)
instance Negatable k => Negatable (En 4 k) where
    negate (E4 x) = E4 (negate x)
instance Negatable k => Negatable (En 5 k) where
    negate (E5 x) = E5 (negate x)
    
        
instance (Multiplicative k) =>  LeftAction k (En 2 k) where
    k *. (E2 (x1,x2)) = E2  (k*x1,k*x2)
instance (Multiplicative k) =>  LeftAction k (En 3 k) where
    k *.  (E3 (x1,x2,x3)) = E3 (k*x1,k*x2,k*x3)    
instance (Multiplicative k) =>  LeftAction k (En 4 k) where
    k *.  (E4 (x1,x2,x3,x4)) = E4 (k*x1,k*x2,k*x3,k*x4)
instance (Multiplicative k) =>  LeftAction k (En 5 k) where
    k *.  (E5 (x1,x2,x3,x4,x5)) = E5 (k*x1,k*x2,k*x3,k*x4,k*x5)                            

instance (OrderedRing k) => EuclideanVector 1 k where 
    euvector (UniTuple1 x) = E1 $ UniTuple1 x
instance (OrderedRing k) => EuclideanVector 2 k where 
    euvector (x1,x2) = E2 (x1,x2)
instance (OrderedRing k) => EuclideanVector 3 k where 
    euvector (x1,x2,x3) = E3 (x1,x2,x3)
instance (OrderedRing k) => EuclideanVector 4 k where 
    euvector (x1,x2,x3,x4) = E4 (x1,x2,x3,x4)
instance (OrderedRing k) => EuclideanVector 5 k where 
    euvector (x1,x2,x3,x4,x5) = E5 (x1,x2,x3,x4,x5)
    
instance (AbelianGroup k) => AbelianGroup (En 1 k)
instance (AbelianGroup k) => AbelianGroup (En 2 k)
instance (AbelianGroup k) => AbelianGroup (En 3 k)
instance (AbelianGroup k) => AbelianGroup (En 4 k)
instance (AbelianGroup k) => AbelianGroup (En 5 k)
        
instance (OrderedRing k) => NatBasisElement 1 1 k where 
    en = E1 $ UniTuple1 one    
instance (OrderedRing k) => NatBasisElement 2 1 k where 
    en = E2 (one,zero)
instance (OrderedRing k) => NatBasisElement 2 2 k where 
    en = E2 (zero,one)    
instance (OrderedRing k) => NatBasisElement 3 1 k where 
    en = E3 (one,zero,zero)
instance (OrderedRing k) => NatBasisElement 3 2 k where 
    en = E3 (zero,one,zero)
instance (OrderedRing k) => NatBasisElement 3 3 k where 
    en = E3 (zero,zero,one)
instance (OrderedRing k) => NatBasisElement 4 1 k where 
    en = E4 (one,zero,zero,zero)
instance (OrderedRing k) => NatBasisElement 4 2 k where 
    en = E4 (zero,one,zero,zero)
instance (OrderedRing k) => NatBasisElement 4 3 k where 
    en = E4 (zero,zero,one,zero)
instance (OrderedRing k) => NatBasisElement 4 4 k where 
    en = E4 (zero,zero,zero,one)
instance (OrderedRing k) => NatBasisElement 5 1 k where 
    en = E5 (one,zero,zero,zero,zero)
instance (OrderedRing k) => NatBasisElement 5 2 k where 
    en = E5 (zero,one,zero,zero,zero)
instance (OrderedRing k) => NatBasisElement 5 3 k where 
    en = E5 (zero,zero,one,zero,zero)
instance (OrderedRing k) => NatBasisElement 5 4 k where 
    en = E5 (zero,zero,zero,one,zero)
instance (OrderedRing k) => NatBasisElement 5 5 k where 
    en = E5 (zero,zero,zero,zero, one)

instance (OrderedRing k) => NatBasis 1 k where    
    natbasis = FiniteBasisSet $ FiniteSet [en @1 @1]
instance (OrderedRing k) => NatBasis 2 k where
    natbasis = FiniteBasisSet $ FiniteSet [en @2 @1, en @2 @2]
instance (OrderedRing k) => NatBasis 3 k where
    natbasis = FiniteBasisSet $ FiniteSet [en @3 @1, en @3 @2, en @3 @3]    
instance (OrderedRing k) => NatBasis 4 k where
    natbasis = FiniteBasisSet $ FiniteSet [en @4 @1, en @4 @2, en @4 @3, en @4 @4]        
instance (OrderedRing k) => NatBasis 5 k where
    natbasis = FiniteBasisSet $ FiniteSet [en @5 @1, en @5 @2, en @5 @3, en @5 @4, en @5 @5]

-- instance (NatBasis n k, Euclidean n k v) => Basis k v (FiniteBasisSet v) where
--     basis  _ = natbasis @ n

-- instance forall n k v s.  (KnownNat n, NatBasis n k, Euclidean n k v, s ~ FiniteBasisSet v) => FiniteBasis n k v s
    