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
    Zippable(..), 
    Groupable(..),
    tree,
    nonempty,
    bag,
    exclude,

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
import qualified Data.Sequence as Seq

-- | Creates a nonempty collection
nonempty::a -> [a] -> NonEmpty a
nonempty = (:|)

-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 

-- / Characterizes a container 'c' dispensing elements of type 'Item' c 
class (IsList c) => Container c where
    -- | Constructs a container from a list of elements, equivalent to 'fromList'
    contain::[Item c] -> c
    contain = fromList

    -- Produces the contained elements, equivalent to 'toList'
    contents::c -> [Item c]
    contents = toList

-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = Bag.fromList
        
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

    
instance Triple (Set a) (Set b) (DisjointUnion (Set a) (Set b)) where
    trip3 a b = DisjointUnion (a, b)
    trip2 (DisjointUnion (a,b)) = b
    trip1 (DisjointUnion (a,b)) = a

-- | Container instances    
-------------------------------------------------------------------------------
instance (Ord a) => Container (Set a) where
    contain = fromList
    contents = toList

instance (Eq a) => Container [a] where
    contain x = x
    contents = id     
    
instance Container (NonEmpty a)

instance (Ord k) => Container (Map k v)

instance Container (Vector a)

instance Container (Seq a) where
    contain x = Seq.fromList x
    contents = toList    

instance (Ord a) => Container (Bag a)
    
    
-- | IsList instances    
-------------------------------------------------------------------------------


instance Iterable (Stream a) where
    iterate = Stream.iterate    
    
    
instance Groupable [a] where
    groups = List.groupBy
    
instance Zippable [a] [b] [c] where
    type LeftZip [a] [b] [c] = [a]
    type RightZip [a] [b] [c] = [b]
    type Zipped [a] [b] [c] = [c]    
    type Zipper [a] [b] [c] = a -> b -> c

    zip = List.zipWith
    
countDistinct::Bag a -> Int
countDistinct = Bag.distinctSize



instance (Ord a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag
            
