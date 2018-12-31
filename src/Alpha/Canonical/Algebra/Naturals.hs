-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Canonical.Algebra.Naturals
(
    NatK(..), NatKPair(..), NatKSpan(..),     
    KnownNatPair(..), KnownNatTriple(..), KnownNatQuad(..), 
    nats,
    natKpair,  natKspan,
    natK, nat2, natK2, nat3, natK3, nat4, natK4,
    natKadd, natKsub, natKmod, natKdiv, natKmul, natKinc, natKdec
) where

import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Divisive
import Alpha.Canonical.Algebra.Modular
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Common
import Alpha.Canonical.Algebra.Hetero
import Alpha.Canonical.Algebra.Successive
import Alpha.Canonical.Algebra.Span

import Alpha.Canonical.Algebra.Exponential
import Alpha.Canonical.Algebra.Field


-- Unifies type naturals and value-level integers
newtype NatK k = NatK Integer    
    deriving (Eq,Ord)

-- | Represents a pair of type-level natural numbers
newtype NatKPair m n = NatKPair (NatK m, NatK n)
    deriving(Eq,Ord)
    
-- | Represents a contiguous range of type-level natural numbers
newtype NatKSpan m n = NatKSpan (NatKPair m n)
    deriving(Eq)

-- | Alias for a 2-tuple of 'KnownNat' constraints    
type KnownNatPair m n = (KnownNat m, KnownNat n)

-- | Alias for a 3-tuple of 'KnownNat' constraints
type KnownNatTriple m n p = (KnownNatPair m n, KnownNat p)

-- | Alias for a 4-tuple of 'KnownNat' constraints
type KnownNatQuad m n p q = (KnownNatPair m n, KnownNatPair p q)

type instance Individual (NatKPair m n) = Integer
type instance Individual (NatKSpan m n) = Integer
type instance Summed (NatK m) (NatK n) = NatK(m + n)
type instance Multiplied (NatK m) (NatK n) = NatK(m * n)
type instance Subtracted (NatK m) (NatK n) = NatK(m - n)
type instance Divided (NatK m) (NatK n) = NatK (m / n)
type instance Modulo (NatK m) (NatK n) = NatK (m % n)
type instance Decrement (NatK m) = NatK (m - 1)
type instance Increment (NatK m) = NatK (m + 1)
type instance Raised (NatK m) (NatK n) = NatK (m ^ n)
type instance Infimum (NatKSpan m n) = NatK m
type instance Supremum (NatKSpan m n) = NatK n
type instance Span (NatK m) (NatK n) = NatKSpan m n
--type instance Factored (NatKPair m n) = (NatK m, NatK n)
type instance Paired (NatK m) (NatK n) (NatKPair m n) = NatKPair m n

-- | Computes the value-level representation of a type-level nat
natK::forall k. KnownNat k => NatK k
natK  = natg @k |> NatK

-- | Computes the value-level representation of a pair of type-level nats
natK2::forall m n. KnownNatPair m n => (NatK m, NatK n)
natK2 = (natK @m, natK @n)

-- | Computes a pair of 'Int' values corresponding to a pair of type-level nats
nat2::forall m n. KnownNatPair m n => (Int,Int)
nat2 = (nat @m, nat @n)

-- | Computes the value-level representation of a triple of type-level nats
natK3::forall m n p. KnownNatTriple m n p => (NatK m, NatK n, NatK p)
natK3 = (natK @m, natK @n, natK @p)

-- | Computes a triple of 'Int' values corresponding to 3 type-level nats
nat3::forall m n p. KnownNatTriple m n p => (Int,Int,Int)
nat3 = (nat @m, nat @n, nat @p)

-- | Computes the value-level representation of a quad of type-level nats
natK4::forall m n p q. KnownNatQuad m n p q => (NatK m, NatK n, NatK p, NatK q)
natK4 = (natK @m, natK @n, natK @p, natK @q)

-- | Computes a 4-tuple of 'Int' values corresponding to 4 type-level nats
nat4::forall m n p q. KnownNatQuad m n p q => (Int,Int,Int,Int)
nat4 = (nat @m, nat @n, nat @p, nat @q)

natKpair::forall m n. KnownNatPair m n => NatKPair m n
natKpair = NatKPair (natK @m, natK @n)

natKspan::forall m n. KnownNatPair m n => NatKSpan m n
natKspan = NatKSpan (natKpair @m @n)

natKadd::forall m n. (KnownNatPair m n) => NatK (m + n)
natKadd =  natK @m >+< natK @n

natKsub::forall m n. (KnownNatPair m n) => NatK (m - n)
natKsub =  natK @m >-< natK @n

natKmod::forall m n. (KnownNatPair m n) => NatK (m % n)
natKmod =  natK @m >%< natK @n

natKdiv::forall m n. (KnownNatPair m n) => NatK (m / n)
natKdiv =  natK @m >/< natK @n

natKmul::forall m n. (KnownNatPair m n) => NatK (m * n)
natKmul =   natK @m >*< natK @n

natKdec::forall m. (KnownNat m ) => NatK (m - 1)
natKdec =  natK @m |> (>--<)

natKinc::forall m. (KnownNat m) => NatK (m + 1)
natKinc = natK @m |> (>++<)

-- | Produces a list of lenth m of integral values of the form [0,...,m-1]
nats::forall m i. (KnownNat m, Integral i) => [i]
nats = [0 .. upper] where
    upper = (nat @m) - 1 |> integral

instance forall k. KnownNat k => Formattable (NatK k) where
    format (NatK k) = format k <> (" (NatK " <> format (nat @k) <> ")") 

instance forall k. KnownNat k =>  Show (NatK k) where
    show k  = string (format k)

instance forall m n. (KnownNatPair m n) =>  Pairing (NatK m) (NatK n) (NatKPair m n) where    
    pair _ _ = NatKPair (natK @m, natK @n)
    first _ = natK @m
    second _ = natK @n
    
-- instance forall m n. (KnownNatPair m n) =>  Factorable (NatKPair m n) where    
--     type Factored (NatKPair m n) = (NatK m, NatK n)
--     factor (NatKPair (m,n)) = (m,n)
--     unfactor (m,n) = NatKPair (m,n)
        
instance forall m n. (KnownNatPair m n) =>  Spanned (NatK m) (NatK n) where    
    span::NatK m -> NatK n -> Span (NatK m) (NatK n)
    span (NatK m) (NatK n) = NatKSpan $ NatKPair $ (natK @m, natK @n)
    {-# INLINE span #-}
    
instance forall m n. (KnownNatPair m n) => Membership (NatKSpan m n)  where        
    members (NatKSpan (NatKPair (NatK m, NatK n))) = [m .. n]
    
instance forall m. (KnownNat m) => Decrementable (NatK m) where    
    dec::NatK m -> Decrement (NatK m) 
    dec (NatK m)  = m - 1 |> NatK
    {-# INLINE dec #-}

instance forall m. (KnownNat m) => Incrementable (NatK m) where    
    inc::NatK m -> Increment (NatK m) 
    inc (NatK m)  = m + 1 |> NatK
    {-# INLINE inc #-}
    
instance forall m n. (KnownNatPair m n) =>  Bimodular (NatK m) (NatK n) where    
    bimod::NatK m -> NatK n -> Modulo (NatK m) (NatK n)
    bimod (NatK m) (NatK n) = m % n |> NatK
    {-# INLINE bimod #-}

instance forall m n. (KnownNatPair m n) =>  Bimultiplicative (NatK m) (NatK n) where    
    bimul::NatK m -> NatK n -> Multiplied(NatK m) (NatK n)
    bimul (NatK m) (NatK n) = m * n |> NatK
    {-# INLINE bimul #-}

instance forall m n. (KnownNatPair m n) =>  Biadditive (NatK m) (NatK n) where    
    biadd::NatK m -> NatK n -> Summed (NatK m) (NatK n)
    biadd (NatK m) (NatK n) = m + n |> NatK
    {-# INLINE biadd #-}

instance forall m n. (KnownNatPair m n) =>  Bisubtractive (NatK m) (NatK n) where    
    bisub::NatK m -> NatK n -> Subtracted (NatK m) (NatK n)
    bisub (NatK m) (NatK n) = m - n |> NatK
    {-# INLINE bisub #-}

instance forall m n. (KnownNatPair m n) =>  Bidivisive (NatK m) (NatK n) where    
    bidiv::NatK m -> NatK n -> Divided (NatK m) (NatK n)
    bidiv (NatK m) (NatK n) = m * n |> NatK
    {-# INLINE bidiv #-}

instance forall m n. (KnownNatPair m n) =>  BiLT (NatK m) (NatK n) where    
    lt (NatK m) (NatK n) = m < n 
    {-# INLINE lt #-}

instance forall m n. (KnownNatPair m n) =>  BiGT (NatK m) (NatK n) where    
    gt (NatK m) (NatK n) = m > n 
    {-# INLINE gt #-}

instance forall m n. (KnownNatPair m n) =>  BiEQ (NatK m) (NatK n) where    
    eq (NatK m) (NatK n) = m == n 
    {-# INLINE eq #-}
        
instance forall m n. (KnownNatPair m n) =>  BiLTEQ (NatK m) (NatK n) where    
    lteq (NatK m) (NatK n) = m <= n 
    {-# INLINE lteq #-}

instance forall m n. (KnownNatPair m n) =>  BiGTEQ (NatK m) (NatK n) where    
    gteq (NatK m) (NatK n) = m >= n 
    {-# INLINE gteq #-}
    
-- | Provides evidence for the claim that two nats can be compared    
instance forall m n. (KnownNatPair m n) => BiComparable (NatK m) (NatK n) 
    
