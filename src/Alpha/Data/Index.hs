{-# LANGUAGE DataKinds #-}

module Alpha.Data.Index where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product


import qualified Data.List as List

data Index (i::Nat) = Index
    
instance forall i. KnownNat i => Show(Index i) where
    show x = show val where
        val = nat @i

type family Index2 (i::Nat) (j::Nat) where
    Index2 i j = (Index i, Index j)

type family Index3 (i::Nat) (j::Nat) (k::Nat) where
    Index3 i j k = (Index i, Index j, Index k)
    
index2::forall (i::Nat) (j::Nat). Index2 i j
index2 = (Index @i, Index @j)

index3::forall (i::Nat) (j::Nat) (k::Nat). Index3 i j k
index3 = (Index @i, Index @j, Index @k)

