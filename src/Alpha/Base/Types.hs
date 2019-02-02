{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Types
(
    Coercible(..), 
    Coercion(..),
    Data(..),
    Typeable(..), 
    Generic(..),
    Generic1(..),    
    Type,
    Default(..),

    TypeRep, typerep, valtype,
    TypeRepG, typerepg, 
    TyCon,TyConInfo(..), tycon, 
    DataType, datatype,
    Proxy(..), proxy, 

    SomeSymbol, KnownSymbol, symstr, symtext, someSymbolVal,
    KnownNat, SomeNat, Nat, nat, natg, natrange,natpair,
    
    
    typeof,
    
    datavaltype,
    typeOf, 
    
    type (+), type (-), type (*), Mod(..), type (%), Div(..), type (/), type (^),        
)
where

import Data.Coerce(Coercible(..))
import Data.Data(Data(..),DataType)
import Data.Default(Default(..))
import Data.Kind(Type)
import Data.Proxy
import Data.Typeable(Typeable(..),Proxy(..),TypeRep,typeOf,typeRep)
import Type.Reflection(typeRepTyCon,someTypeRepTyCon, SomeTypeRep)
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
import Data.Text(Text)
import GHC.Show(Show)
import Alpha.Base.Alias

import qualified Data.Text as Text
import qualified Type.Reflection as Reflect

type TypeRepG a = Reflect.TypeRep a
type TyCon = Reflect.TyCon

-- | Describes a type constructor
newtype TyConInfo 
    = TyConInfo (
        Text, -- ^ The package name
        Text, -- ^ The module name
        Text  -- ^ The constructor name
        )
    deriving (Data,Typeable,Generic)

typerep::forall a. Typeable a => SomeTypeRep
typerep = Reflect.someTypeRep (Proxy::Proxy a)

typerepg::forall a. Typeable a => TypeRepG a
typerepg = Reflect.typeRep @a

-- | Retrieves the 'DataType' metadata for the type 'a'
datatype::forall a. (Data a, Default a) => DataType
datatype =   dataTypeOf (def @a)

-- | Retrieves the 'TypeRep' metadata for the type 'a'
typeof:: forall a. (Typeable a, Default a) => TypeRep 
typeof = typeOf (def @a)

tycon'::forall a. Typeable a => TyCon
tycon' = someTypeRepTyCon (typerep @a)

tycon::forall a. Typeable a => TyConInfo
tycon = info where
    c = tycon' @a
    info = TyConInfo (
        Text.pack (Reflect.tyConPackage c),
        Text.pack (Reflect.tyConModule c),
        Text.pack (Reflect.tyConName c))

valtype::Typeable a => a -> TypeRep
valtype = typeOf 

datavaltype::Data a => a -> DataType
datavaltype = dataTypeOf

-- Modulus infix operator synonm
type (%) m n = Mod m n
infixl 5 %

-- Division infix operator synonm
type (/) m n = Div m n
infixl 7 /

proxy::Proxy n
proxy = Proxy
{-# INLINE proxy #-}

-- | Produces a string for a symbol
symstr::forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- | Produces a text for a symbol
symtext :: forall s. KnownSymbol s => Text
symtext =  Text.pack (symbolVal @s Proxy)

-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = fromIntegral (natVal (proxy @m))
{-# INLINE nat #-}

-- | Computes the (generic) integral value corresponding to a type-level nat
natg::forall m i. (KnownNat m, Integral i) => i
natg = fromIntegral (natVal (proxy @m))
{-# INLINE natg #-}

-- | Constructs a list of integers [0..n - 1] where n is a type-level natural
natrange::forall n i. (KnownNat n, Integral i) => [i]
natrange =[0 .. (sub' (natg @n) 1)]
{-# INLINE natrange #-}

-- | Constructs a pair of integers determined by type-level naturals
natpair::forall m n i. (KnownNat m, KnownNat n, Integral i) => (i,i)
natpair = (natg @m, natg @n)
{-# INLINE natpair #-}

--natrange2::forall m n i (KnownNat m, KnownNat n, Integral i,)