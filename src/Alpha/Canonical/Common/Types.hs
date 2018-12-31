-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Types
(
    S,
    L,
    HashMap(..),
    Op(..),
) where
import Alpha.Base as X

data S 
data L 

type family HashMap d a b = r | r -> a b
type instance HashMap S a b = StrictHashMap a b
type instance HashMap L a b  = LazyHashMap a b


-- | Captures duality at the type-level
type family Op a = r | r -> a
type instance Op (StrictMap a b) = LazyMap a b
type instance Op (LazyMap a b) = StrictMap a b

