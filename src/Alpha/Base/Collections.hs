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
    Bag,
    Map, associate,
    LazyMap,
    IsList(..),   
    Stream,
    Ix,
    NonEmpty(..),
    Vector,

)
where
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Map.Strict(Map)
import Data.Tree(Tree,Forest)
import Data.Set(Set)
import Data.Sequence(Seq)
import Data.Stream.Infinite
import Data.List.NonEmpty(NonEmpty((:|)) )
import Data.Set(Set)
import Data.Ix(Ix(..))
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty((:|)) )
import Data.Foldable
import GHC.Exts(IsList(..))
import Data.Ord
import Data.Eq(Eq)
import GHC.Generics(Generic)
import Data.Semigroup(Semigroup)
import Data.Monoid(Monoid)
import Data.Foldable(Foldable)
import Data.Data(Data)
import Control.DeepSeq(NFData)
import Control.Category((.))

import qualified Data.Map.Lazy as LM
import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Alpha.Base.Newtype

type Bag a = MS.MultiSet a
type LazyMap k v = LM.Map k v


-- | Produces an associative array for a list of key-value pairs
associate::(Ord k) => [(k,v)] -> Map k v
associate = Map.fromList
