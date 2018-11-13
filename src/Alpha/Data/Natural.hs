{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.Natural
(
    Sized(..),
    Natural, NatRange, 
    Zero, One, Next, Add, Sub,Mul,
    natval, natrange, nat, natdec, natinc, natadd, natsub, natmul, natmod
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

class (KnownNat n) => Sized n where
    size::Int
    size = natval @n

data NatRange (i::Nat) (j::Nat) = NatRange (Natural i, Natural j)    

type Zero = Natural 0
type One =  Natural 1
type Next n = Natural (n + 1)
type Add m n = Natural (m + n)
type Sub m n  = Natural (m - n)
type Mul m n = Natural (m * n)

natrange::forall (i::Nat) (j::Nat). (KnownNat i, KnownNat j) => NatRange i j
natrange = NatRange (x, y) where 
    x = fromIntegral(natVal (Proxy @i))
    y = fromIntegral(natVal (Proxy @j))

-- Extracts the value encoded at the type-level
natval::forall n i. (KnownNat n, Integral i) => i
natval = fromInteger( natVal (Proxy @n) )
        
-- | Defines a nat
nat::forall m (n::Nat). (Integral m) =>  m -> Natural n
nat = Natural . fromIntegral 

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
