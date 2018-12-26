-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Elementary.Structure
(
    Structure(..),
    DiscreteStructure(..),
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Sets

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence



class Structure (a::Constraint) (i::Type) where
    
-- | Classifies a type that structures a set of individuals according
-- to axioms of the particular structure. This is in contradistinction
-- to a 'Set' which is merely a collection of elements. Note also that
-- a structure parametrized by a does not necessarily lead to a collection
-- of elements of type 'a'. This is true only in the degenerate case - which
-- also happens to be the default
class Structure a i => DiscreteStructure a i where

