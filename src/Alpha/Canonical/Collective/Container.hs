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
    Headed(..),
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

    
    
-- | The elements of a tree are projected onto a list
type instance Appended (Tree a) = [a]


instance Appendable (Tree a) where
    append = Tree.flatten
        
tree::(b -> (a, [b])) -> b-> Tree a
tree = Tree.unfoldTree
                
    
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

instance (Ord a) => Container (FiniteSet a) where
    contain = fromList
    contents = toList
    
instance (Ord a) => Filterable (FiniteSet a) where
    filter p s = FiniteSet $  Set.filter p  (unwrap s)
    
instance Pairing (FiniteSet a) (FiniteSet b) (DisjointUnion (FiniteSet a) (FiniteSet b)) where
    pair a b = DisjointUnion (a, b)
    first (DisjointUnion (a,b)) = a
    second (DisjointUnion (a,b)) = b
            