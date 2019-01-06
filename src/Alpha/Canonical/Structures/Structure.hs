-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Structures.Structure
(
    Structure(..),
    StructureN(..),
    Morphic(..),
    Mor(..),
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Set

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence

type family Sig (i::Nat) = r | r -> i
type instance Sig 1 = Type -> Constraint
type instance Sig 2 = Type -> Type -> Constraint
type instance Sig 3 = Type -> Type -> Type -> Constraint

type family SigN (i::Nat) (n::Nat) = r | r -> i
type instance SigN 1 n = Nat -> Type -> Constraint
type instance SigN 2 n = Nat -> Type -> Type -> Constraint
type instance SigN 3 n = Nat -> Type -> Type -> Type -> Constraint

   
class Structure i (c::Sig i) where

class StructureN i (c::SigN i n) where
    
-- Defines a family of structure-preserving functions (for groups, modules, etc)
-- See https://en.wikipedia.org/wiki/Morphism
type family Mor (s::Constraint) a b :: Type
type instance Mor (Structure n s) a b = a -> b

class Morphic (f::Type -> Type -> Type) s a b  where
    morphism::f a b -> (a -> b)
    
