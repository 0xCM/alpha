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
    Range,
    span
)
where
import Data.Range.Range
import Data.Ord

-- | Creates an inclusively-bound range
span::(Ord a) => a  -> a -> Range a
span min max = SpanRange min max



