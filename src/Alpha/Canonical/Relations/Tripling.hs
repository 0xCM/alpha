-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Tripling
(
    Tripled(..), Triple(..)
) where


-- Codifies a ternary relationship among three types:
-- The first type, the second type and the unification of
-- the two types as a pair
type family Tripled a b c = r | r -> a b c    

-- Codifies, at the value level, a ternary relatioship among
-- three types: The first type, second type and the unification 
-- of the two types as a pair
class Triple a b c where

    -- | Constructs a triple
    trip3::a -> b -> Tripled a b c

    --- | Extracts the second of the paired elements
    trip2::Tripled a b c -> b

    --- | Extracts the first of the paired elements
    trip1::Tripled a b c -> a
    
    swap::(Triple b a c) => Tripled a b c -> Tripled b a c
    swap x = trip3 (trip2 x) (trip1 x)
    
