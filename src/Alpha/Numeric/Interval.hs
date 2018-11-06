module Alpha.Numeric.Interval 
(
    Interval, (+/-),interval,width
)
where

import Numeric.Interval(Interval, (...), (+/-),interval,width)
import qualified Numeric.Interval as I
import Alpha.Base
import Alpha.Canonical hiding( (...))

instance (Ord a) => Container (Interval a) a where
    contains = I.member
    singleton = I.singleton

instance (Eq a) => Nullary (Interval a) where
    zero = I.empty

instance Infimum (Interval a) a where
    infimum = I.inf

instance Supremum (Interval a) a where
    supremum = I.sup

instance (Ord a) => Spanned (Interval a) a where
    span = (...)
    