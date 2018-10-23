-----------------------------------------------------------------------------
-- | Basic type-level support for symbols
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alpha.TypeLevel.Symbols 
(
    typeSymbol,
    typeSymbols,
    symstr

) where
import Data.Functor
import qualified GHC.TypeLits as TL
import qualified GHC.Natural as TN
import Data.Type.Equality hiding (type (==), apply)
import Alpha.Base
    
symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- | Forms a 'SomeSymbol' value from a 'String'
typeSymbol::String -> SomeSymbol
typeSymbol = TL.someSymbolVal

-- | Forms a list of 'SomeSymbol' values from a list of'String'
typeSymbols::[String] ->[SomeSymbol]
typeSymbols s = fmap typeSymbol s
    