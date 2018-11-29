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
    
-- instance Ord a => Container (R.Range a) a where
--     singleton = R.SingletonRange

-- instance (Ord a, Enum a) =>  Setwise [R.Range a] a where
--     union = R.union
--     intersect = R.intersection
--     delta = R.difference
    