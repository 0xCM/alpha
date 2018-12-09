-----------------------------------------------------------------------------
-- | Fundamental container-related constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Containers
(
    Container(..),
    FiniteContainer(..),
    SeqStream(..),
    bag, tree, set,
)
where

import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations

import Alpha.Canonical.Collective.Discrete
import Alpha.Canonical.Collective.Sequential

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree

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


-- | Characterizes a container that holds a finite number of elements    
class (Counted c, Container c) => FiniteContainer c where

class (Sequential (Stream a), Weavable a (Stream a), Iterable (Stream a) ) => SeqStream a where    

    -- Constructs a sream that emits the elements
    -- of a list, cyling over said elements indefinitely
    -- if the list is finite
    cycle::[a] -> Stream a

    -- Constructs a stream via opaque function calls
    blackbox::(() -> a) -> Stream a
    
-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = Bag.fromList

tree::(b -> (a, [b])) -> b-> Tree a
tree = Tree.unfoldTree

-- Constructs a set from a list
set::Ord a => [a] -> Set a
set = Set.fromList

countDistinct::Bag a -> Int
countDistinct = Bag.distinctSize



testTree = tree (\x -> (x, [1..x])) 5

instance Counted (Set a) where
    count = fromIntegral . Set.size
    
instance Counted (Bag a) where
    count = fromIntegral . Bag.size    

instance IsList (Stream a) where
    type Item (Stream a) = a
    toList = Stream.takeWhile (\_ -> True)
    fromList [x] = Stream.cycle [x]

instance SeqStream (Stream s)  where
    cycle (x:xs) = Stream.cycle(x :| xs)
    blackbox f = Stream.iterate (\_ -> f ()) (f())        
    
instance Container (Stream e) where
    singleton x = Stream.cycle [x]
    contain [x] = Stream.cycle [x]
    contents s = Stream.takeWhile (\_ -> True) s
        
instance (Ord a) => FiniteContainer (Set a) where    
    
instance (Ord a, PartialOrder a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag
    
instance (Ord a, PartialOrder a) => Container (Bag a) where        
    contain = Bag.fromList
    contents = Bag.toList
    
instance (Eq a) => Container [a] where
    contain x = x
    contents = id
    
instance (Ord a) => Container (Set a) where
    contain = Set.fromList
    contents = Set.toList
    
instance Container (Seq a) where
    contain x = Seq.fromList x
    contents = toList    