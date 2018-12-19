-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Partition
(
    Span(..),    
    IntegralSpan(..),
    Partition(..),
    Spanned(..),
    Partitioner(..)

) where
import Alpha.Base
import Alpha.Canonical.Elementary
import Alpha.Canonical.Functions
import Alpha.Canonical.Relations
import qualified Data.List as List
import qualified Numeric.Interval as I

type family Span a b
type instance Span (Min a) (Max a) = IntegralSpan a    
type instance Span (Interval a) (Interval a) = Interval a

type instance Element (IntegralSpan a) = a

-- | Characterizes a type that contains a relatively contiguous
-- set of values bound by least and greatest values
class (Ord a, Ord b) => Spanned a b where
    
    -- | Creates a contiguous span between supplied endpoints
    span::a -> b -> Span a b
    
    -- | The span operator, an infix synonym for 'span'
    (...)::a -> b -> Span a b
    (...) = span
    infixl 5 ...

class Partition a where
    breakpoints::a -> [Element a]
        
class Partitioner a where
    partition::Int -> [a] -> [[a]]
    partition width = List.takeWhile (not . List.null) . fmap (List.take width) . List.iterate (List.drop width)    
instance Partitioner a    

newtype IntegralSpan a = IntegralSpan [a]

instance (Ord a, Integral a) => Partition (IntegralSpan a)  where
    breakpoints (IntegralSpan l) = l
    
instance (Ord a, Integral a) => Spanned (Min a) (Max a) where
    span (Min min) (Max max) = IntegralSpan [min .. max]

instance (Ord a) => Spanned (Interval a) (Interval a) where
    span i1 i2 = (infimum i1) I.... (supremum i2)
                