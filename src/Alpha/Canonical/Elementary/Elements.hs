-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Elementary.Elements
(
    Set(..),
    Finite(..), 
    Infinite(..),
    Counted(..),
    Countable(..),
    Listing(..),
    Vectored(..),
    Unlisted(..),
    InvariantSet(..)
)
where
import Alpha.Base
import Alpha.Canonical.Elementary.Element

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence

-- | Classifies a type that can be interpreted as a 
-- constructive or non-constructive set of any cardinality
class Set a where

-- | Classifies a type that can be interpreted as a 
-- constructive or non-constructive finite set
class Finite a where


-- | Classifies a type with which can be interpreted
-- as an constructive or non-constructive infinite set
class Infinite a where    

-- | Classifies a type that can be interpreted as a 
-- constructive or non-constructive set that be placed
-- in bijecive correspondence with a proper subset of 
-- the natural numbers or with the set of natural numbers
-- itself
class Countable a where

-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n


class Listing a where
    list::a -> [Element a]
    
class Vectored a where
    vector::a -> Vector (Element a)    

-- | Characterizes a type that can be hydrated by an element list
class (Listing a) => Unlisted a  where
    unlist::[Element a] -> a
    
-- | Classifies a type with which a set of invariant values are associated
class InvariantSet s where
    invariants::[s]

-- A counted set is countable
instance (Counted a) => Countable a

instance Set (InvariantSet a)
    
instance Infinite [a]
instance Finite (Bag a)    
instance Infinite (Tree a)
instance Finite (Map a b)
instance Infinite (LazyMap a b)
instance Finite (Vector a)
instance Infinite (Stream a)
instance Finite(Set.Set a)

instance Listing (Map a b) where
    list = Map.toList                        
