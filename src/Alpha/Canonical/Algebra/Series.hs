-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Series
(
    Series(..), 
    series
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative

-- | Represents a mathematical series
newtype Series i t = Series (O2 t, IxRange i, IxTerm i t) 
    deriving(Generic,Typeable)
instance Newtype(Series i t)

-- | Constructs a mathematical series
series::(Integral i) => O2 t -> (i,i) -> IxTerm i t -> Series i t
series op (min,max) f = Series (op, IxRange (min, max), f)