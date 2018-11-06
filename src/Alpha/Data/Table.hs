{-# LANGUAGE DataKinds #-}
module Alpha.Data.Table where

import Alpha.Base

newtype Cell (r::Nat) (c::Nat) a = Cell a
