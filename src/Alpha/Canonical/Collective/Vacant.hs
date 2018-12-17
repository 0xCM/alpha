-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Vacant where

import Alpha.Base
import Alpha.Canonical.Common
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
        
instance Vacant [a] where
    empty = []
    null = List.null

instance (Eq a) => Vacant (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Vacant (Map k v) where
    empty = Map.empty
    null = Map.null

instance Vacant (Vector a) where
    empty = Vector.empty
    null = Vector.null
    
instance Vacant (Bag a) where
    empty = Bag.empty
    null = Bag.null

