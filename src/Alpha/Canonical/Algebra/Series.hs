module Alpha.Canonical.Algebra.Series
(
    Series(..), 
    SeriesKind(..), 
    series
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative


-- | Represents a mathematical series
newtype Series i t = Series (SeriesKind, O2 t, IxRange i, IxTerm i t) 
    deriving(Generic)
instance Newtype(Series i t)

-- | Classifies a mathematical series
data SeriesKind =
      ProductSeries
    | SummationSeries
    deriving(Eq,Enum,Show)

-- | Constructs a mathematical series
series::(Integral i, Additive t, Multiplicative t) => SeriesKind -> (i,i) -> IxTerm i t -> Series i t
series SummationSeries (min,max) f = Series (SummationSeries, (+), IxRange (min, max), f)
series ProductSeries (min,max) f = Series (ProductSeries, (*), IxRange (min,max), f)

