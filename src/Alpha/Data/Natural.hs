{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.Natural
(
    Sized(..),
    Natural, NatPair, 
    Zero, One, Next, Add, Sub,Mul,
    natural, natpair, natdec, natinc, natadd, natsub, natmul, natmod
)
where

import qualified GHC.Natural as N
import GHC.Real as R
import GHC.TypeNats (type (<=), type (<=?), type(*), type (+), type (^), type (-), Mod)
import GHC.TypeLits(natVal)
import GHC.Num hiding(Natural)
import Alpha.Base
import Alpha.TypeLevel.Proxy
import Alpha.Canonical(Formattable(..))

-- Unifies type and value-level naturals
newtype Natural k = Natural N.Natural
    deriving (Num)

class KnownNat n => Sized n where
    size::Int
    size = s where
        n = natural @n
        s = integral n
        integral::Natural k -> Int
        integral (Natural x) = fromIntegral x
        

-- | Determines a pair of typed natural numbers    
data NatPair (i::Nat) (j::Nat) 
    = NatPair (Natural i) (Natural j)

type Zero = Natural 0
type One =  Natural 1
type Next n = Natural (n + 1)
type Add m n = Natural (m + n)
type Sub m n  = Natural (m - n)
type Mul m n = Natural (m * n)
    
-- | Constructs a typed nat
natural :: forall n. KnownNat n => Natural n
natural = fromIntegral (natVal (proxy @n))

-- | Constructs a typed nat pair
natpair::forall r c. (KnownNat r, KnownNat c) => NatPair r c
natpair = NatPair (natural @r) ( natural @c)

-- | Decrements a nat
natdec::forall (n::Nat). Natural(n+1) -> Natural n
natdec (Natural i) = Natural (i - 1)

-- | Increments a nat
natinc::forall (n::Nat). Natural(n) -> Natural (n+1)
natinc (Natural i) = Natural (i + 1)

-- | Adds two nats
natadd::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m + n)
natadd (Natural i) (Natural j) = Natural (i + j)

-- | Nat subtraction
natsub::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m - n)
natsub (Natural i) (Natural j) = Natural (i - j)

-- | Nat multiplication
natmul::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m * n)
natmul (Natural i) (Natural j) = Natural (i * j)

-- | Nat mod
natmod::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (Mod m n)
natmod (Natural i) (Natural j) = Natural (i `R.mod` j)

instance Show (Natural k) where
    show (Natural i) = show i 
