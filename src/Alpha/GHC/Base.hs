-----------------------------------------------------------------------------
-- | Common GHC.* imports
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Alpha.GHC.Base
(
    module X,
    Generic(..),
    Generic1(..),    
    Enum(..),
    Num, 
    Integer,
    Integral,
    Symbol(..),
    KnownSymbol(..),
    SomeSymbol(..),
    Nat,    
    mod,
    symbolVal,
    Bounded(..),
    fromIntegral,
    
)
where


import GHC.Show as X
import GHC.TypeLits
import GHC.Real
import GHC.Num
import GHC.Enum
import GHC.Types
import GHC.TypeNats(Mod)
import GHC.Generics(Generic(..),Generic1(..))






