{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Text
(
    String, Text,
    IsString,
    Show(..), 
    Read(..), readEither, readMaybe, read, 
    Char,chr,intToDigit,
    SomeSymbol, KnownSymbol, Symbol, SymbolPair, symbolVal, symbolVal', symbol,
)
where

import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Real(Integral,fromIntegral)
import Text.Show
import Data.Proxy
import Data.Function
import Data.Text(Text)
import Data.String(IsString(..))
import Text.Read(readEither,readMaybe,read)
import Data.String(String,IsString)
import Data.Char(Char,intToDigit)
import qualified Data.Char as Char
import GHC.TypeLits(symbolVal, symbolVal',SomeSymbol,KnownSymbol)
import GHC.Types(Symbol)

type SymbolPair s t = (KnownSymbol s, KnownSymbol t)

-- Produces the value for a symbol
symbol::forall s. KnownSymbol s => String
symbol = symbolVal (Proxy @s)

chr::Integral n => n -> Char
chr = Char.chr . fromIntegral

