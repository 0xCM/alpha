-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Interval
(
    interval
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical


interval::(Ord a, Semiring a) => a -> a -> Interval a
interval = interval'