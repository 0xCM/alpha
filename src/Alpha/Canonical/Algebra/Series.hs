module Alpha.Canonical.Algebra.Series
(
    Series(..), 
    SeriesKind(..), 
    series
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Semiring
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative


-- | Represents a mathematical series
newtype Series i t = Series (SeriesKind, O2 t, IndexRange i, IndexedTerm i t) 
    deriving(Generic)
instance Newtype(Series i t)

-- | Classifies a mathematical series
data SeriesKind =
      ProductSeries
    | SummationSeries
    deriving(Eq,Enum,Show)

-- | Constructs a mathematical series
series::(Integral i, Semiring t) => SeriesKind -> (i,i) -> (i->t) -> Series i t
series SummationSeries i t = Series (SummationSeries, (+), IndexRange i, term t)
series ProductSeries i t = Series (ProductSeries, (*), IndexRange i, term t)

-- instance (Integral i, Semiring t, Ord t)  => Computable (Series i t) where
--     type Computed (Series i t) = t
--     compute (Series (k, f, r, t)) = aggregation where
    
--         expansion = (unwrap t) <$> toList  (setspec r)
--         identity = ifelse (k == SummationSeries) zero one
--         aggregation = reduce identity f (toList expansion)