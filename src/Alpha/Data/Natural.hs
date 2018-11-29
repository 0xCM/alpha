{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.Natural
(
    TypedNat, 
    Zero, One, Next, Add, Sub,Mul,
    natural, natdec, natinc, natadd, natsub, natmul, natmod
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
newtype TypedNat k = TypedNat N.Natural
    deriving (Num)

class KnownNat n => Sized n where
    size::Int
    size = s where
        n = natural @n
        s = integral n
        integral::TypedNat k -> Int
        integral (TypedNat x) = fromIntegral x
        
type Zero = TypedNat 0
type One =  TypedNat 1
type Next n = TypedNat (n + 1)
type Add m n = TypedNat (m + n)
type Sub m n  = TypedNat (m - n)
type Mul m n = TypedNat (m * n)
    
-- | Constructs a typed nat
natural :: forall n. KnownNat n => TypedNat n
natural = fromIntegral (natVal (proxy @n))

-- | Decrements a nat
natdec::forall (n::Nat). TypedNat(n+1) -> TypedNat n
natdec (TypedNat i) = TypedNat (i - 1)

-- | Increments a nat
natinc::forall (n::Nat). TypedNat(n) -> TypedNat (n+1)
natinc (TypedNat i) = TypedNat (i + 1)

-- | Adds two nats
natadd::forall (m::Nat) (n::Nat). TypedNat m -> TypedNat n -> TypedNat (m + n)
natadd (TypedNat i) (TypedNat j) = TypedNat (i + j)

-- | Nat subtraction
natsub::forall (m::Nat) (n::Nat). TypedNat m -> TypedNat n -> TypedNat (m - n)
natsub (TypedNat i) (TypedNat j) = TypedNat (i - j)

-- | Nat multiplication
natmul::forall (m::Nat) (n::Nat). TypedNat m -> TypedNat n -> TypedNat (m * n)
natmul (TypedNat i) (TypedNat j) = TypedNat (i * j)

-- | Nat mod
natmod::forall (m::Nat) (n::Nat). TypedNat m -> TypedNat n -> TypedNat (Mod m n)
natmod (TypedNat i) (TypedNat j) = TypedNat (i `R.mod` j)

instance Show (TypedNat k) where
    show (TypedNat i) = show i 
