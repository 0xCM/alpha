{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Collections
(
    HashSet, 
    HashMap, 
    Tree, Forest,
    Seq, 
    Set,
    Bag,
    Map, associate,
    IsList(..),   
    Stream,
    Ix(..),
    NonEmpty(..),
    Vector,

)
where
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Tree(Tree,Forest)
import Data.Set(Set)
import Data.Sequence(Seq)
import Data.Stream.Infinite
import Data.List.NonEmpty(NonEmpty((:|)) )
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Ix(Ix(..))
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty((:|)) )

import GHC.Exts(IsList(..))
import Data.Ord


import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as Map

type Bag a = MS.MultiSet a

-- | Produces an associative array for a list of key-value pairs
associate::(Ord k) => [(k,v)] -> Map k v
associate = Map.fromList
