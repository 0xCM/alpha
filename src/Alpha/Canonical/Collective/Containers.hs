-----------------------------------------------------------------------------
-- | Fundamental container-related constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Collective.Containers
(
    Container(..),
    FiniteContainer(..),
    SequentialStream(..),
    bag, tree, 
    exclude

)
where

import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations

import Alpha.Canonical.Collective.Container
import Alpha.Canonical.Collective.Discrete
import Alpha.Canonical.Collective.Sequential

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Text as Text



class (Sequential (Stream a), Weavable a (Stream a), Iterable (Stream a) ) => SequentialStream a where    

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


-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 

countDistinct::Bag a -> Int
countDistinct = Bag.distinctSize


testTree = tree (\x -> (x, [1..x])) 5


instance Collapsible (Tree a) where
    collapse = Tree.flatten
        
instance Collapsible [[a]] where
    collapse = List.concat    
    
instance Collapsible [Text] where
    collapse = Text.concat        
        
instance Counted [a] where
    count = fromIntegral . List.length
    
instance Counted (Bag e) where
    count = fromIntegral . Bag.size
            
instance IsList (Stream a) where
    type Item (Stream a) = a
    toList = Stream.takeWhile (\_ -> True)
    fromList [x] = Stream.cycle [x]

instance SequentialStream (Stream s)  where
    cycle (x:xs) = Stream.cycle(x :| xs)
    blackbox f = Stream.iterate (\_ -> f ()) (f())        
    
instance Container (Stream e) where
    singleton x = Stream.cycle [x]
    contain [x] = Stream.cycle [x]
    contents s = Stream.takeWhile (\_ -> True) s
            
instance (Ord a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag
    
instance (Ord a) => Container (Bag a)

instance (Eq a) => Container [a] where
    contain x = x
    contents = id
        
instance Container (Seq a) where
    contain x = Seq.fromList x
    contents = toList    

instance Container (Vector a)

instance (Ord k) => Container (Map k v)

instance (Eq a) => Filterable (Stream a) where
    filter = Stream.filter
    
instance (Eq a) => Filterable [a] where
    filter = List.filter
    
instance Filterable (Seq a) where
    filter = Seq.filter