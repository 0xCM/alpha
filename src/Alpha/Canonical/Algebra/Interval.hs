-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Interval
(
    interval
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Semiring


interval::(Ord a, Semiring a) => a -> a -> Interval a
interval = interval'