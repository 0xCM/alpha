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
    FacetValue(..), facetVal

) where
import Data.Functor
import GHC.TypeLits
import Data.Type.Equality hiding (type (==), apply)
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators
import qualified Data.Text as T

data FacetValue f v = FacetValue v

        
facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val
    
instance  (Formattable v, Faceted f v) => Formattable (FacetValue f  v) where
    format (FacetValue v) =  format v
    
instance Faceted "length" Int where
    
