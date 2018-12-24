-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Collective.Container
(
    Container(..),
    Filterable(..),
    Setwise(..),
    Headed(..),
    Vacant(..),
    Sequential(..),
    Mappable(..),    
    Zippable(..), 
    Groupable(..),
    tree,
) where

import Alpha.Base
import Alpha.Canonical.Relations

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Numeric.Interval as Interval

-- / Characterizes a container 'c' dispensing elements of type 'Item' c 
class (IsList c) => Container c where
    -- | Constructs a container from a list of elements, equivalent to 'fromList'
    contain::[Item c] -> c
    contain = fromList

    -- Produces the contained elements, equivalent to 'toList'
    contents::c -> [Item c]
    contents = toList

    -- | Constructs a container with exactly one element
    singleton::Item c -> c
    singleton e = contain [e]



-- | Characterizes a container holding elements that can be 
-- filtered via a unary predicate    
class (Container c) => Filterable c where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1 (Item c) -> c -> c

    -- | Selects a single element that satisfies a predicate
    single::P1 (Item c) -> c -> Item c
    single p c =  List.head $ contents $ filter p c  

-- | Characterizes types whose values can be treated as sets
class (Container c) => Setwise c where
    -- The union operator
    union::c -> c -> c
    -- The intersection operator
    intersect::c -> c -> c
    -- The set difference operator
    delta::c -> c -> c
    -- The set membership test operator
    isSubset::Bool -> c -> c -> Bool
    
-- | Classifies a structure that can be partitioned into two sets:
-- A singleton set containing the "first" element and another set containing
-- the remainder
class Headed a where
    -- | Retrives the first item in the sequence
    head::a -> Individual a

        -- | Skips the first item of the sequence and returns the remainder
    tail::a -> a

-- / Characterizes a type for which a canonical and unique vacant/void/empty
-- value exists
class Vacant a where

    -- | Exhibits the canonical empty value
    empty::a

    -- | Determines whether a given value is the canonical
    -- 'empty' value
    null::a -> Bool

instance (Eq a) => Vacant (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Vacant (Map k v) where
    empty = Map.empty
    null = Map.null
    

class Mappable c a b where    
    type Mapped c a b
    map::(a -> b) -> c -> Mapped c a b
    
-- | The elements of a tree are projected onto a list
type instance Appended (Tree a) = [a]


instance Eq a => Set (Tree a)


instance Appendable (Tree a) where
    append = Tree.flatten
        
tree::(b -> (a, [b])) -> b-> Tree a
tree = Tree.unfoldTree
                
class (Headed a) => Sequential a  where 

    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> a -> a

        -- | Returns elements until a supplied predicate is disatisfied
    while::P1 (Individual a) -> a -> a
    
    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::P1 (Individual a) -> a -> (a, a)

    splitAt::(Integral n) => n -> a -> (a, a)

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> a -> a
    
-- Characterizes a composition and pairing of heterogenous values
class Zippable a b c where
    
    -- The left source type
    type LeftZip a b c

    -- The right source type
    type RightZip a b c

    -- The result type
    type Zipped a b c

    -- The combining operator
    type Zipper a b c
    
    zip::Zipper a b c -> LeftZip a b c -> RightZip a b c -> Zipped a b c
    
class Groupable c where
    groups::(Individual c -> Individual c -> Bool) -> c -> [[Individual c]]
