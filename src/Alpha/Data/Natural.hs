{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.Natural
(
    TypedNat(..),     
    NatPair(..), nat2,
    NatTriple(..), nat3,
    NatQuad(..), nat4,
    NatSpan(..), natspan,
    NatPaired(..), natpair,
    nat, natdec, natinc, natadd, natsub, natmul
)
where

import Alpha.Base
import Alpha.TypeLevel.Proxy
import Alpha.Canonical
import Alpha.Data.Numbers


-- Unifies type naturals and value-level integers
newtype TypedNat k = TypedNat Int
    deriving (Num,ToInt)

-- | Represents a contiguous range of natural numbers inclusively between m and n
newtype NatSpan m n = NatSpan [Int]    

-- | Represents a pair of natural numbers (m,n)
newtype NatPaired m n = NatPaired (Int,Int)

-- | Alias for a pair of 'KnownNat' constraints    
type NatPair m n = (KnownNat m, KnownNat n)  

-- | Alias for a tuple of 'KnownNat' constraints
type NatTriple m n p = (NatPair m n, KnownNat p)

-- | Alias for a quadruple of 'KnownNat' constraints    
type NatQuad m n p q = (NatPair m n, NatPair p q)
    
-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = natVal (proxy @m) |> int

-- | Computes a pair of 'Int' values corresponding to a pair of type-level nats
nat2::forall m n. NatPair m n => (Int,Int)
nat2 = (nat @m, nat @n)

-- | Computes a triple of 'Int' values corresponding to 3 type-level nats
nat3::forall m n p. NatTriple m n p => (Int,Int,Int)
nat3 = (nat @m, nat @n, nat @p)

-- | Computes a 4-tuple of 'Int' values corresponding to 4 type-level nats
nat4::forall m n p q. NatQuad m n p q => (Int,Int,Int,Int)
nat4 = (nat @m, nat @n, nat @p, nat @q)

-- | Decrements a nat
natdec:: forall n. (KnownNat n) => TypedNat(n - 1)
natdec = TypedNat $ nat @n - 1

-- | Increments a nat
natinc::forall n. (KnownNat n) => TypedNat(n + 1)
natinc = TypedNat $ nat @n + 1

-- | Adds two nats
natadd::forall m n. (NatPair m n) => TypedNat (m + n)
natadd = TypedNat $ nat @m + nat @n

-- | Nat subtraction
natsub::forall m n. (NatPair m n) => TypedNat (m - n)
natsub = TypedNat $ nat @m - nat @n

-- | Nat multiplication
natmul::forall m n. (NatPair m n) => TypedNat (m * n)
natmul = TypedNat $ nat @m * nat @n

natmod::forall m n. (NatPair m n) => TypedNat (m % n)
natmod = TypedNat ((nat @m) % (nat @n ))

-- | Constructs a 'Span' of natural numbers
natspan::forall m n. NatPair m n => NatSpan m n
natspan = NatSpan [(nat @m)..(nat @n)]

-- | Constructs a pair of natural numbers
natpair::forall m n. NatPair m n => NatPaired m n
natpair = NatPaired <| pair (nat @m) (nat @n)

instance Show (TypedNat k) where
    show (TypedNat i) = show i 

