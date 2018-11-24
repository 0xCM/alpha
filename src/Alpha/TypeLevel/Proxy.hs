{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.TypeLevel.Proxy
(
    proxy, Proxy, Value, Witness(..)
)
where

import Alpha.Base
import GHC.TypeLits

proxy:: Proxy n
proxy = Proxy
    
type family Value k :: Type    

class Witness (w :: k) where
    value :: Value k

type instance Value Bool = Bool
type instance Value Symbol = String
type instance Value Nat = Integer
    
instance KnownSymbol s => Witness (s :: Symbol) where
    value =  symbolVal (proxy @s)
    
instance KnownNat n => Witness (n :: Nat) where
    value = natVal (proxy @n)
