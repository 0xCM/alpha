{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.NatK
(
    NatK(..), NatKPair(..), NatKSpan(..),     
    KnownNatPair(..), KnownNatTriple(..), KnownNatQuad(..), 

    natKpair,  natKspan,
    natK, nat2, natK2, nat3, natK3, nat4, natK4,
    natKadd, natKsub, natKmod, natKdiv, natKmul, natKinc, natKdec
) where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Conversion
    
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

type instance Element (NatKPair m n) = Integer
type instance Element (NatKSpan m n) = Integer
type instance Bisum (NatK m) (NatK n) = NatK(m + n)
type instance Biproduct (NatK m) (NatK n) = NatK(m * n)
type instance Delta (NatK m) (NatK n) = NatK(m - n)
type instance Quotient (NatK m) (NatK n) = NatK (m / n)
type instance Modulo (NatK m) (NatK n) = NatK (m % n)
type instance Decrement (NatK m) = NatK (m - 1)
type instance Increment (NatK m) = NatK (m + 1)
type instance Raised (NatK m) (NatK n) = NatK (m ^ n)
type instance Infimum (NatKSpan m n) = NatK m
type instance Supremum (NatKSpan m n) = NatK n
type instance Span (NatK m) (NatK n) = NatKSpan m n
type instance Factored (NatKPair m n) = (NatK m, NatK n)
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

natKinc::forall m. (KnownNat m ) => NatK (m + 1)
natKinc = natK @m |> (>++<)

instance forall k. KnownNat k => Formattable (NatK k) where
    format (NatK k) = format k |> prepend (" (NatK " <> format (nat @k) <> ")") 

instance forall k. KnownNat k =>  Show (NatK k) where
    show k  = string (format k)


instance forall m n. (KnownNatPair m n) =>  Pairing (NatK m) (NatK n) (NatKPair m n) where    
    pair _ _ = NatKPair (natK @m, natK @n)
    first _ = natK @m
    second _ = natK @n
    
instance forall m n. (KnownNatPair m n) =>  Factorable (NatKPair m n) where    
    factor (NatKPair (m,n)) = (m,n)
        
instance forall m n. (KnownNatPair m n) =>  Spanned (NatK m) (NatK n) where    
    span::NatK m -> NatK n -> Span (NatK m) (NatK n)
    span (NatK m) (NatK n) = NatKSpan $ NatKPair $ (natK @m, natK @n)
    {-# INLINE span #-}
    
instance forall m n. (KnownNatPair m n) => Partition (NatKSpan m n)  where        
    points (NatKSpan (NatKPair (NatK m, NatK n))) = [m .. n]
    
instance forall m. (KnownNat m) => Decrementable (NatK m) where    
    dec::NatK m -> Decrement (NatK m) 
    dec (NatK m)  = m - 1 |> NatK
    {-# INLINE dec #-}

instance forall m. (KnownNat m) => Incrementable (NatK m) where    
    inc::NatK m -> Increment (NatK m) 
    inc (NatK m)  = m + 1 |> NatK
    {-# INLINE inc #-}
    
instance forall m n. (KnownNatPair m n) =>  HModular (NatK m) (NatK n) where    
    hmod::NatK m -> NatK n -> Modulo (NatK m) (NatK n)
    hmod (NatK m) (NatK n) = m % n |> NatK
    {-# INLINE hmod #-}

instance forall m n. (KnownNatPair m n) =>  HMultiplicative (NatK m) (NatK n) where    
    hmul::NatK m -> NatK n -> Biproduct (NatK m) (NatK n)
    hmul (NatK m) (NatK n) = m * n |> NatK
    {-# INLINE hmul #-}

instance forall m n. (KnownNatPair m n) =>  HAdditive (NatK m) (NatK n) where    
    hadd::NatK m -> NatK n -> Bisum (NatK m) (NatK n)
    hadd (NatK m) (NatK n) = m + n |> NatK
    {-# INLINE hadd #-}

instance forall m n. (KnownNatPair m n) =>  HSubtractive (NatK m) (NatK n) where    
    hsub::NatK m -> NatK n -> Delta (NatK m) (NatK n)
    hsub (NatK m) (NatK n) = m - n |> NatK
    {-# INLINE hsub #-}

instance forall m n. (KnownNatPair m n) =>  HDivisive (NatK m) (NatK n) where    
    hdiv::NatK m -> NatK n -> Quotient (NatK m) (NatK n)
    hdiv (NatK m) (NatK n) = m * n |> NatK
    {-# INLINE hdiv #-}

instance forall m n. (KnownNatPair m n) =>  Powered (NatK m) (NatK n) where    
    raise (NatK m) (NatK n) = (natural m) ^ (natural n) |> fromNatural |> NatK
    {-# INLINE raise #-}
    
instance forall m n. (KnownNatPair m n) =>  IsLT (NatK m) (NatK n) where    
    lt (NatK m) (NatK n) = m < n 
    {-# INLINE lt #-}

instance forall m n. (KnownNatPair m n) =>  IsGT (NatK m) (NatK n) where    
    gt (NatK m) (NatK n) = m > n 
    {-# INLINE gt #-}

instance forall m n. (KnownNatPair m n) =>  IsEQ (NatK m) (NatK n) where    
    eq (NatK m) (NatK n) = m == n 
    {-# INLINE eq #-}
        
instance forall m n. (KnownNatPair m n) =>  IsLTEQ (NatK m) (NatK n) where    
    lteq (NatK m) (NatK n) = m <= n 
    {-# INLINE lteq #-}

instance forall m n. (KnownNatPair m n) =>  IsGTEQ (NatK m) (NatK n) where    
    gteq (NatK m) (NatK n) = m >= n 
    {-# INLINE gteq #-}
    
-- | Provides evidence for the claim that two nats can be compared    
instance forall m n. (KnownNatPair m n) => Comparable (NatK m) (NatK n) 
    
