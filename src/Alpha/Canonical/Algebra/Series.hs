-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Series
(
    SeriesSpec(..), 
    ProductSeries(..),
    SummationSeries(..),
    series,
    summation,
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative

    

-- | Represents a mathematical series
newtype SeriesSpec i t = SeriesSpec (O2 t, IxRange i, IxTerm i t) 
    deriving(Generic,Typeable)
instance Newtype(SeriesSpec i t)

newtype ProductSeries i t = ProductSeries (SeriesSpec i t)
    deriving(Generic,Typeable)
instance Newtype(ProductSeries i t)

newtype SummationSeries i t = SummationSeries (SeriesSpec i t)
    deriving(Generic,Typeable)
instance Newtype(SummationSeries i t)

-- | Constructs a mathematical series
series::(Integral i) => O2 t -> (i,i) -> IxTerm i t -> SeriesSpec i t
series op (min,max) f = SeriesSpec (op, IxRange (min, max), f)

summation::(Integral i, Additive t) => (i,i) -> IxTerm i t -> SummationSeries i t
summation (min,max) f = series (+) (min,max) f |> SummationSeries

instance (Integral i, Nullary t) => Computable (SummationSeries i t) where
    type Computed (SummationSeries i t) = t
    compute (SummationSeries (SeriesSpec (op, range, term))) = result where
        f = unwrap term |> snd
        items = f <$> (members range)
        result = items |> reduce zero op
       
