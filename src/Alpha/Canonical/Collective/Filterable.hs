-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Filterable
(
    Filterable(..),
    exclude, 
) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Collective.Containers

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream

-- | Characterizes a container holding elements that can be 
-- filtered via a unary predicate    
class (Container c) => Filterable c where

    -- | Excludes elements that don't satisfy a predicate
    filter::UnaryPredicate (Item c) -> c -> c

    -- | Selects a single element that satisfies a predicate
    single::UnaryPredicate (Item c)-> c -> Item c
    single p c =  List.head $ contents $ filter p c  
    
instance (Eq a) => Filterable (Stream a) where
    filter = Stream.filter
    
instance (Eq a) => Filterable [a] where
    filter = List.filter
    
instance Filterable (Seq a) where
    filter = Seq.filter
        
instance (Ord a) => Filterable (Set a) where
    filter = Set.filter

-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 
