-----------------------------------------------------------------------------
-- | Common GHC.* imports
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Alpha.GHC.Base
(
    Show(..),
    Generic(..),
    Generic1(..),    
    Bounded(..),

    Enum(..),
    Num, Integer, Integral,
    Symbol(..), KnownSymbol(..), SomeSymbol(..), 
    KnownNat(..), SomeNat(..), Nat(..),
    mod,
    symbolVal,
    fromIntegral,
    undefined,
    ($),(<=), (>=),
    id
    

)
where


import GHC.Show(Show(..))
import GHC.TypeLits
import GHC.Real((/), (^), (^^), (%), Real(..), Integral(..), RealFrac(..), Fractional(..), fromIntegral)
import GHC.Num
import GHC.Enum
import GHC.Types
import GHC.Base(($),(<=), (>=),undefined, id)
import GHC.TypeNats(Mod)
import GHC.Generics(Generic(..),Generic1(..))
import GHC.TypeLits(KnownNat(..), SomeNat(..), Nat(..))






