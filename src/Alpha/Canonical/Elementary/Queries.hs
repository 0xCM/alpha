-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Queries
(
    Universal(..),
    Existential(..),
    Queryable(..),

) where

import Alpha.Canonical.Common
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

class Queryable a where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1 (Individual a) -> a -> [Individual a]

    -- | Selects a single element, if any, that satisfies a predicate
    single::P1 (Individual a) -> a -> Maybe (Individual a) 
    single p src = ifelse (List.length filtered == 1) (Just $ List.head filtered) Nothing  
        where
            filtered = filter p src
    
instance Universal [a] where
    all = List.all
            
instance Existential [a] where
    any = List.any
    
instance Queryable [a] where
    filter pred source = List.filter pred source
    