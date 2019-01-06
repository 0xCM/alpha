-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Common
(
    module X,
    Vacuous(..),
    Differential(..),
    Containment(..),
    Unionizable(..),
    Intersectable(..),    
    Singletary(..),
    Tabular(..),
    Transposable(..),

) where
import Alpha.Canonical.Common as X
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.List as List


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
    
class Unionizable a where        
    -- The union operator
    union::a -> a -> a 

    unions::[a] -> a
    default unions::(Vacuous a, Unionizable a) => [a] -> a
    unions u = reduce empty union u
    
    
class Intersectable a  where
    intersect::a -> a -> a

    
class Differential a where
    diff::a -> a -> a
        
-- / Characterizes a type for which a canonical and unique Vacatable/void/empty
-- value exists
class Vacuous a where

    -- | Exhibits the canonical empty value
    empty::a

    -- | Determines whether a given value is the canonical
    -- 'empty' value
    null::a -> Bool

class Containment a where
    isSubset::Bool -> a -> a -> Bool
    
instance (Eq a) => Vacuous (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Vacuous (Map k v) where
    empty = Map.empty
    null = Map.null
        
instance Transposable [[a]] where
    type Transposed [[a]] = [[a]]
    transpose = List.transpose

instance Singletary [a] where
    singleton a = [a]
        