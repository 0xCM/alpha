-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Appendable
(
    Appended(..), Appendable(..)
)
where

import Alpha.Base
import Alpha.Canonical.Elementary
import Alpha.Canonical.Functions
import Alpha.Canonical.Common

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag

-- | Defines a family of type-level functions with the intent
-- of projecting nested a sequence of elements to a (relatively)
-- non-nested sequence of elements. An instance need not be
-- Element-invariant
type family Appended a

-- | A list of element lists is appended to produce a list of elements
type instance Appended [[a]] = [a]

-- | A list of elements is appended to produce a single element
type instance Appended [a] = a

-- | The elements of a tree are projected onto a list
type instance Appended (Tree a) = [a]


-- Classifies a type that can be transformed into an 'Appended' value
class Appendable a where
    append::a -> Appended a

instance Appendable (Tree a) where
    append = Tree.flatten
        
instance Appendable [[a]] where
    append = List.concat    
    
instance Appendable [Text] where
    append = Text.concat        
    