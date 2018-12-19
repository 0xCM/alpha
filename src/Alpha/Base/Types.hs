{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Types
(
    Coercible(..), Coercion(..),
    Data(..),Typeable(..), 
    Generic(..),
    Generic1(..),    
    Type,
    Default(..),

    Proxy(..), proxy, 
    KnownSymbol, symstr,
    KnownNat,  nat, natg, 
    SomeNat, Nat, 
    SomeSymbol, someSymbolVal,
    datatype,typeof,
    
    type (+), type (-), type (*), Mod(..), type (%), Div(..), type (/), type (^),        
)
where

import Data.Coerce(Coercible(..))
import Data.Data(Data(..),DataType)
import Data.Default(Default(..))
import Data.Kind(Type)
import Data.Proxy
import Data.Typeable(Typeable(..),Proxy(..),TypeRep,typeOf)
import Data.Type.Coercion
import Data.Default(Default(def))
import GHC.Generics(Generic(..),Generic1(..))
import GHC.TypeLits(type (+), type (-), type (*), type (^), Mod(..), Div(..))
import GHC.TypeLits(KnownNat, KnownSymbol, symbolVal, SomeNat, Nat,natVal, natVal')
import GHC.TypeLits(SomeSymbol,someSymbolVal)
import GHC.Real(Integral,fromIntegral)
import Data.String(String)
import Data.Int(Int)
import Data.Data(Data(..),)  



-- | Retrieves the 'DataType' metadata for the type 'a'
datatype:: forall a. (Data a, Default a) => DataType
datatype =   dataTypeOf (def @a)

-- | Retrieves the 'TypeRep' metadata for the type 'a'
typeof:: forall a. (Typeable a, Default a) => TypeRep
typeof = typeOf (def @a)


-- Modulus infix operator synonm
type (%) m n = Mod m n
infixl 5 %

-- Division infix operator synonm
type (/) m n = Div m n
infixl 7 /

proxy:: Proxy n
proxy = Proxy
{-# INLINE proxy #-}

-- | Produces a string for a symbol
symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = fromIntegral (natVal (proxy @m))
{-# INLINE nat #-}

-- | Computes the (generic) integral value corresponding to a type-level nat
natg::forall m i. (KnownNat m, Integral i) => i
natg = fromIntegral (natVal (proxy @m))
{-# INLINE natg #-}
