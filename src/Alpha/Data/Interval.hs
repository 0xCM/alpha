module Alpha.Data.Interval
(
    Interval, (+/-),interval,width
) where
import Alpha.Base
import Alpha.Canonical hiding( (...))    
import Numeric.Interval(Interval, (...), (+/-),interval,width)
import qualified Numeric.Interval as I

type instance Extremum (Interval a) = a
type instance Span (Interval a) (Interval a) = Interval a

instance (Eq a) => Nullary (Interval a) where
    zero = I.empty

instance Infimal (Interval a) where
    infimum = I.inf

instance Supremal (Interval a) where
    supremum = I.sup

instance (Ord a) => Spanned (Interval a) (Interval a) where
    span i1 i2 = (infimum i1) ... (supremum i2)
    
    