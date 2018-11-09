-----------------------------------------------------------------------------
-- | Range - a contiguous interval
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Range 
(
    R.Range
)
where
import qualified Data.Range.Range as R
import Data.Ord
import Alpha.Base
import Alpha.Canonical

instance (Ord a) => Spanned (R.Range a) a where
    span min max = R.SpanRange min max    

instance OrderedEnum a => Unionizable [R.Range a] where
    union = R.union

instance OrderedEnum a => Intersectable [R.Range a] where
    intersect = R.intersection
    
instance OrderedEnum a => Diffable [R.Range a] where
    delta = R.difference
    
instance Ord a => Container (R.Range a) a where
    contains c e = R.inRange e c
    singleton = R.SingletonRange