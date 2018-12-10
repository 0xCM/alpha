-----------------------------------------------------------------------------
-- | Support for type-level natural numbers, derivative from Galois bv-sized and 
-- parameterized-utils libraries; see LICENSE
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Alpha.Types.Nats
(
    NatRepr(..), addNat, repVal, knownNat 
)
where
import Alpha.Base
import Alpha.Canonical

import Data.Type.Equality as Equality
import Data.Proxy as Proxy
import qualified GHC.Natural as TN
import qualified GHC.TypeLits as TL
import Alpha.Types.Some
import Alpha.Types.Common
import Alpha.Data.Conversion

newtype NatRepr (n::Nat) = NatRepr {
   natValue :: Integer
  } deriving (Hashable)

    
maxInt :: Integer
maxInt = integer (maxBound :: Int)

knownNat :: forall n . KnownNat n => NatRepr n
knownNat = NatRepr (natg @n)
      
-- | Return the value of the nat representation.
repVal :: NatRepr n -> Int
repVal (NatRepr i) | i < maxInt = fromInteger i
                     | otherwise = error "Width is too large."

addNat :: NatRepr m -> NatRepr n -> NatRepr (m+n)
addNat (NatRepr m) (NatRepr n) = NatRepr (m+n)

instance Eq (NatRepr m) where
    _ == _ = True

instance TestEquality NatRepr where
    testEquality (NatRepr m) (NatRepr n)
        | m == n = Just (unsafeCoerce Refl)
        | otherwise = Nothing

instance (KnownNat n) => Reifiable n Integer where
    reify = TL.natVal (Proxy::Proxy n)
        