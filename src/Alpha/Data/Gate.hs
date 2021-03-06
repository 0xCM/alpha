-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Data.Gate
(
    Gate(..)
)
where

import Alpha.Canonical
import Alpha.Data.Bit
import Alpha.Data.BitVector

--A model of a logic gate
class Gate (s::Symbol) (n::Nat) where
    bitflow::BitVector n -> Bit

instance Gate "and" 2 where
    bitflow x = (x !! 0) && (x !! 1) |> bit

instance Gate "or" 2 where
    bitflow x = (x !! 0) || (x !! 1) |> bit
    
instance Gate "not" 1 where
    bitflow x = (x !! 0) |> isOn  |> bit
    


