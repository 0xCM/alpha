-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoStarIsType #-}
module Alpha.Base.Collections
(
    HashSet, 
    HashMap, 
    Tree, Forest,
    Seq, 
    Map, 
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
import Data.List.NonEmpty
import Data.Ix(Ix(..))
import Data.Vector(Vector)
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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Alpha.Base.Newtype




