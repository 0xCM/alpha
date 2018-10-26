{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Alpha.Data.Natural where

import qualified GHC.Natural as N
import GHC.Real as R
import GHC.TypeNats (type (<=), type (<=?), type(*), type (+), type (^), type (-), Mod)
import GHC.TypeLits(natVal)
import GHC.Num hiding(Natural)
import Alpha.Base
import Alpha.TypeLevel.Proxy
import Alpha.Canonical(Formattable(..), (<>))

-- Unifies type and value-level naturals
newtype Natural k = Natural N.Natural
    deriving (Num)
    
instance Show (Natural k) where
    show (Natural i) = show i 
    
-- | Defines a net
nat::forall m (n::Nat). (Integral m) =>  m -> Natural n
nat = Natural . fromIntegral 

-- | Decrements a nat
dec::forall (n::Nat). Natural(n+1) -> Natural n
dec (Natural i) = Natural (i - 1)

-- | Increments a nat
inc::forall (n::Nat). Natural(n) -> Natural (n+1)
inc (Natural i) = Natural (i + 1)

-- | Adds two nats
add::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m + n)
add (Natural i) (Natural j) = Natural (i + j)

-- | Nat subtraction
sub::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m - n)
sub (Natural i) (Natural j) = Natural (i - j)

-- | Nat multiplication
mul::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (m * n)
mul (Natural i) (Natural j) = Natural (i * j)

-- | Nat mod
mod::forall (m::Nat) (n::Nat). Natural m -> Natural n -> Natural (Mod m n)
mod (Natural i) (Natural j) = Natural (i `R.mod` j)
