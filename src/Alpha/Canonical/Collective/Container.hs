-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Collective.Container
(
    Container(..),
    FiniteContainer(..),
    Collapsed(..),
    Collapsible(..),
    Filterable(..),
) where

import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Operators
import Alpha.Canonical.Common

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag

type family Collapsed a

type instance Collapsed [[a]] = [a]
type instance Collapsed [a] = a
type instance Collapsed (Tree a) = [a]


-- / Characterizes a container 'c' dispensing elements of type e
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

-- | A container is a set    
instance (IsList c) => Set (Container c)

-- | Characterizes a container that holds a finite number of elements    
class (Finite c, Container c) => FiniteContainer c where

-- Removes a layer of structure 
-- In the case of a monoid, 'reduce' reduces to 'fold', pun intended
class Collapsible a where
    collapse::a -> Collapsed a

-- | Characterizes a container holding elements that can be 
-- filtered via a unary predicate    
class (Container c) => Filterable c where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1 (Item c) -> c -> c

    -- | Selects a single element that satisfies a predicate
    single::P1 (Item c) -> c -> Item c
    single p c =  List.head $ contents $ filter p c  

