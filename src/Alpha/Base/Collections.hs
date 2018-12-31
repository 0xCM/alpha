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
    StrictHashMap, LazyHashMap,
    StrictMap, LazyMap,    
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
import Alpha.Base.Newtype

import qualified Data.Map.Strict as SM
import qualified Data.Map as LM
import qualified Data.Set as Set
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as SHM

type LazyHashMap = LHM.HashMap
type StrictHashMap = SHM.HashMap

type StrictMap = SM.Map
type LazyMap = LM.Map









