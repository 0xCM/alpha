-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Common.Container
(
    tree,
    nonempty,
    bag,

) where

import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Data.Sequence as Seq

-- | Creates a nonempty list
nonempty::a -> [a] -> NonEmpty a
nonempty = (:|)


-- Constructs a bag from a list    
bag::(Ord a) => [a] -> Bag a
bag = Bag.fromList
        
tree::(b -> (a, [b])) -> b-> Tree a
tree = Tree.unfoldTree
                
    
-- *IsList instances
-------------------------------------------------------------------------------    
instance (Ord a) =>  IsList (Bag a) where
    type Item (Bag a) = a
    toList = Bag.toList
    fromList = bag

instance Iterable (Stream a) where
    iterate = Stream.iterate    
            
