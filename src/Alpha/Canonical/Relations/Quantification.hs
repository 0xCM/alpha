-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Quantification
(
    Universal(..),
    Existential(..)
) where

import Alpha.Base
import Alpha.Canonical.Elementary
import qualified Data.List as List
import qualified Data.Set as Set


-- | Characterizes types for which existential questions may posed
-- regarding element containment
class Existential c where
    -- Determines whether any element exists that satisfies a given predicate
    any::(Individual c -> Bool) -> c -> Bool

    -- Determines whether an exlement exists via an equality predicate
    exists::(Eq (Individual c)) => Individual c -> c -> Bool
    exists = any . (==) 
    
class Universal c where
    all::(Individual c -> Bool) -> c -> Bool

instance Universal [a] where
    all = List.all
    
        
instance Existential [a] where
    any = List.any
    


