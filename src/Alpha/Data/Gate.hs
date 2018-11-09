-----------------------------------------------------------------------------
-- | Representation of a logic gate
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Gate where

import Data.Kind(Type)
import GHC.TypeLits
import Data.Bool

import Alpha.Canonical
import Alpha.Data.Bit
import Alpha.Data.Bits
import Alpha.Data.Product
import Alpha.Data.BitVector

-- A model of a logic gate
class Gate (s::Symbol) (n::Nat) where
    type Input n    
    type Input n = BitVector n
    
    process::Input n -> Bit

instance Gate "And" 2 where
    process x = (x .?. 0) && (x .?. 1) |> fromBool

instance Gate "Or" 2 where
    process x = (x .?. 0) || (x .?. 1) |> fromBool
    