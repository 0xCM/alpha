-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}
module Alpha.Canonical.Structures.SymS
(
    KnownSymPair(..)
    
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Domain
    
type KnownSymPair a b = (KnownSymbol a, KnownSymbol b)

