-----------------------------------------------------------------------------
-- | Basic type-level support for symbols
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Data.Symbols 
(
    typeSymbol,
    typeSymbols,
    facetVal

) where
import Data.Functor
import GHC.TypeLits
import Data.Type.Equality hiding (type (==), apply)
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators
import qualified Data.Text as T

data FacetValue f v = FacetValue v

symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- | Forms a 'SomeSymbol' value from a 'String'
typeSymbol::String -> SomeSymbol
typeSymbol = someSymbolVal

-- | Forms a list of 'SomeSymbol' values from a list of'String'
typeSymbols::[String] ->[SomeSymbol]
typeSymbols s = typeSymbol <$> s
        
facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val
    
instance  (Formattable v, Faceted f v) => Formattable (FacetValue f  v) where
    format (FacetValue v) =  format v
    
instance Faceted "length" Int where
    
