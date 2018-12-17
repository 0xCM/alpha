{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Element
(
    Element(..),
    Set(..),
    Finite(..), Infinite(..),
    InvariantSet(..),
    Listing(..),
    Unlisted(..),
    Vectored(..),
    Counted(..)
)
where
import Alpha.Base
import qualified Data.Map as Map
import qualified Data.Set as Set

type family Element a
type instance Element [a] = a
type instance Element (Bag a) = a
type instance Element (Seq a) = a
type instance Element (Stream a) = a
type instance Element (Map a b) = (a,b)
type instance Element (Vector a) = a

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

-- A counted set is countable
instance (Counted a) => Countable a

-- | Classifies a type with which a set of invariant values are associated
class InvariantSet s where
    invariants::[s]
instance Set (InvariantSet a)

class Listing a where
    list::a -> [Element a]
    
class Vectored a where
    vector::a -> [Element a]    

class (Listing a) => Unlisted a  where
    unlist::[Element a] -> a

instance Listing (Map a b) where
    list = Map.toList                        

instance Infinite [a]
instance Finite (Bag a)    
instance Infinite (Tree a)
instance Finite (Map a b)
instance Infinite (LazyMap a b)
instance Finite (Vector a)
instance Infinite (Stream a)
instance Finite(Set.Set a)
    