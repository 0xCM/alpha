-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Collective.Map where

import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Container

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.Map as Map

type instance Individual (Map a b) = (a,b)

    

instance (Ord k) => Container (Map k v)

instance (Formattable k, Formattable v) => Formattable (Map k v) where
    format m = format $ format  <$> (Map.toList m)    

instance (Eq a, Eq b) => SetBuilder (Map a b) (a,b) where
    set = Map.toList

