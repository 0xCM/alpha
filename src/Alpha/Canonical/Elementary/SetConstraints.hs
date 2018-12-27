-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.SetConstraints
(
    Vacant(..),
    SetDifference(..),
    SetContainment(..),
    Unionizable(..),
    Intersectable(..),
    NonEmptySet(..)

) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Individual
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval

class Unionizable a where
    -- The union operator
    union::a -> a -> a
    unions::[a] -> a
    
class Intersectable a  where
    intersect::a -> a -> a

-- / Characterizes a type for which a canonical and unique vacant/void/empty
-- value exists
class Vacant a where

    -- | Exhibits the canonical empty value
    empty::a

    -- | Determines whether a given value is the canonical
    -- 'empty' value
    null::a -> Bool

class SetDifference a where
    diff::a -> a -> a

class SetContainment a where
    isSubset::Bool -> a -> a -> Bool
    
-- | Characterizes a nonempty set    
class NonEmptySet a where
    -- | Retrives the leading (first) item in the set which
    -- is guaranteed to exist
    leading::a -> Individual a    
    
instance (Eq a) => Vacant (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Vacant (Map k v) where
    empty = Map.empty
    null = Map.null
    