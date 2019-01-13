-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Common
(
    module X,
    Singletary(..),
    Tabular(..),
    Transposable(..),    
    Queryable(..),
    Filter(..),

) where
import Alpha.Canonical.Common as X

import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream
import qualified Data.Sequence as Sequence


class Filter a where
    allow::Individual a -> Bool

-- | Characterizes a type that can be constructed from exactly one individual
class Singletary a where
    singleton::Individual a -> a

-- | Characterizes a rectangular data source
class Tabular a where
    rows::a -> [[Individual a]]
    cols::a -> [[Individual a]]
    
class Transposable a where
    type Transposed a
    transpose::a -> Transposed a
            

class Queryable a where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1 (Individual a) -> a -> [Individual a]

    -- | Selects a single element, if any, that satisfies a predicate
    single::P1 (Individual a) -> a -> Maybe (Individual a) 
    single p src = ifelse (List.length filtered == 1) (Just $ List.head filtered) Nothing  
        where
            filtered = filter p src
                        
            
-------------------------------------------------------------------------------            
-- *Transposable instances
-------------------------------------------------------------------------------        
instance Transposable [[a]] where
    type Transposed [[a]] = [[a]]
    transpose = List.transpose

-------------------------------------------------------------------------------            
-- * Singletary instances
-------------------------------------------------------------------------------        

instance Ord k => Singletary (Map k v) where
    singleton (k,v) = fromList [(k,v)]
    
instance Singletary [a] where
    singleton a = [a]

instance Singletary (Seq a) where
    singleton x = fromList [x]

instance Singletary (Vector a) where
    singleton a = Vector.fromList [a]

instance Singletary (Stream a) where
    singleton x = Stream.cycle (x :| [])
        

instance (Ord a) => Singletary (Bag a) where
    singleton a = Bag.fromList [a]
    
                

-------------------------------------------------------------------------------            
-- * Queryable instances
-------------------------------------------------------------------------------            
instance Queryable [a] where
    filter pred source = List.filter pred source
    
instance Queryable (Seq a) where
    filter pred source =  toList source |> List.filter pred

