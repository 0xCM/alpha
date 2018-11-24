-----------------------------------------------------------------------------
-- | Symbol-related type-level utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module Alpha.Types.Symbol
(
    KnownSymbol(..),
    symbol
)
where

import Alpha.Base
import GHC.TypeLits

-- Produces the value for a symbol
symbol::forall s. KnownSymbol s => String
symbol = symbolVal (Proxy @s)
