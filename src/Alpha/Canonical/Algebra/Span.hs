-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Span
(
    Span(..),    
    IntegralSpan(..),
    Spanned(..),


) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Numeric
import qualified Data.List as List
import qualified Numeric.Interval as I

type family Span a b
type instance Span (Min a) (Max a) = IntegralSpan a    
type instance Span (Interval a) (Interval a) = Interval a
type instance Individual (IntegralSpan a) = a

newtype IntegralSpan a = IntegralSpan [a]

-- | Characterizes a type that contains a relatively contiguous
-- set of values bound by least and greatest values
class (Ord a, Ord b) => Spanned a b where
    
    -- | Creates a contiguous span between supplied endpoints
    span::a -> b -> Span a b
    
    -- | The span operator, an infix synonym for 'span'
    (...)::a -> b -> Span a b
    (...) = span
    infixl 5 ...

instance (Ord a, Integral a) => Membership (IntegralSpan a)  where
    members (IntegralSpan l) = l
    
instance (Ord a, Integral a) => Spanned (Min a) (Max a) where
    span (Min min) (Max max) = IntegralSpan [min .. max]

instance (Ord a) => Spanned (Interval a) (Interval a) where
    span i1 i2 = (infimum i1) I.... (supremum i2)                

