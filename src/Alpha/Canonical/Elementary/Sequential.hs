-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Sequential
(
    Headed(..),
    Predicative(..),
    Paged(..),
    Sequential(..),

) where
import Alpha.Canonical.Common

-- | Classifies a structure that can be partitioned into two sets:
-- A singleton set containing the "first" element and another set containing
-- the remainder
class Headed a where
    type Remaining a
    type Remaining a = a

    -- | Retrives the first item in the sequence
    head::a -> Individual a

    -- | Skips the first item of the sequence and returns the remainder
    tail::a -> Remaining a
        
class Predicative a where
    -- | Returns elements until a supplied predicate is disatisfied
    while::P1(Individual a) -> a -> a

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::P1(Individual a) -> a -> (a, a)
    
class Paged a  where 

    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> a -> a


    splitAt::(Integral n) => n -> a -> (a, a)

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> a -> a
    
class (Headed a, Predicative a, Paged a) => Sequential a