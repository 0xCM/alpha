-----------------------------------------------------------------------------
-- | Representation of a logic gate
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Alpha.Data.Gate where


import Alpha.Canonical
import Alpha.Data.BitVector
import Alpha.Data.Bit
import Alpha.Data.Bits

-- A model of a logic gate
class Gate (s::Symbol) (n::Nat) where
    type Input n    
    type Input n = BitVector n
    
    process::Input n -> Bit

instance Gate "and" 2 where
    process x = (x .?. 0) && (x .?. 1) |> bit

instance Gate "or" 2 where
    process x = (x .?. 0) || (x .?. 1) |> bit
    
instance Gate "not" 1 where
    process x = ifelse (x .?. 0) on off
    


