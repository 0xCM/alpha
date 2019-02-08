-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoStarIsType #-}
module Alpha.Base.Collections
(
    module X,
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
import Control.DeepSeq(NFData)
import Control.Monad
import Data.Data(Data,Typeable)
import Data.Eq(Eq)
import Data.Foldable(Foldable)
import Data.Foldable
import Data.Functor
import Data.HashSet(HashSet)
import Data.Ix(Ix(..))
import Data.List.NonEmpty
import Data.Map.Strict(Map)
import Data.Monoid(Monoid)
import Data.Ord
import Data.Set(Set)
import Data.Sequence(Seq)
import Data.Stream.Infinite
import Data.Semigroup(Semigroup)
import Data.Tree(Tree,Forest)
import Data.Vector(Vector)
import GHC.Exts(IsList(..))
import GHC.Generics(Generic)

import Alpha.Base.Newtype as X

import qualified Data.Map.Strict as SM
import qualified Data.Map as LM
import qualified Data.Set as Set
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as SHM
import qualified Data.Vector as Vector

type LazyHashMap = LHM.HashMap
type StrictHashMap = SHM.HashMap

type StrictMap = SM.Map
type LazyMap = LM.Map

