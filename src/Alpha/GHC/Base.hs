-----------------------------------------------------------------------------
-- | Common GHC.* imports
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-----------------------------------------------------------------------------
module Alpha.GHC.Base
(
    Show(..), Read(..),
    Generic,
    Generic1,    
    Bounded,minBound,maxBound,
    Enum,fromEnum,toEnum,
    Num, Integer, Integral, Real, mod,
    Symbol, 
    KnownSymbol, 
    SomeSymbol, 
    KnownNat, 
    SomeNat, 
    Nat,
    IsList(..),
    
    symbolVal,
    fromIntegral,
    undefined,
    ($),
    id,
    natVal, natVal'
    

)
where


import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Real((/), (^), (^^), (%), Real, Integral, mod, RealFrac, Fractional, fromIntegral)
import GHC.Num(Num,Integer)
import GHC.Enum(Enum, fromEnum,toEnum, Bounded,minBound,maxBound)
import GHC.Types(Symbol)
import GHC.Base(($),undefined, id)
import GHC.TypeNats(Mod)
import GHC.Generics(Generic,Generic1)
import GHC.TypeLits(KnownNat, SomeNat, Nat,natVal, natVal', symbolVal, symbolVal',SomeSymbol,KnownSymbol)
import GHC.Exts(IsList(..))






