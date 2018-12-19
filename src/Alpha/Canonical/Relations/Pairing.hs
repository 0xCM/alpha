-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Pairing
(
    Paired(..), Pairing(..),
) where


-- Codifies a ternary relationship among three types:
-- The first type, the second type and the unification of
-- the two types as a pair
type family Paired a b c = r | r -> a b c    

-- Codifies, at the value level, a ternary relatioship among
-- three types: The first type, second type and the unification 
-- of the two types as a pair
class Pairing a b c where

    -- | Pairs two elements
    pair::a -> b -> Paired a b c

    --- | Extracts the first of the paired elements
    first::Paired a b c -> a
    
    --- | Extracts the second of the paired elements
    second::Paired a b c -> b

    swap::(Pairing b a c) => Paired a b c -> Paired b a c
    swap x = pair (second x) (first x)
    