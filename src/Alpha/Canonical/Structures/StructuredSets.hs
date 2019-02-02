-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Structures.StructuredSets
(
    GeneratingSet(..), 
    IndependentSet(..),
    BasisSet(..),
)
where

import Alpha.Canonical.Algebra

-- | Represents a generating set for  a space in the following
-- sense: every element of the space can be expressed as a 
-- formal sum of the elements in the set
-- See 
-- https://en.wikipedia.org/wiki/Generator_(mathematics)
-- https://en.wikipedia.org/wiki/Free_module
newtype GeneratingSet a = GeneratingSet (Set a)
    deriving(Eq, Ord, Generic, Data, Typeable, Membership, Discrete)
    deriving(Formattable,Show) via (Set a)
instance Newtype (GeneratingSet a)
type instance Individual (GeneratingSet a) = a

-- | Represents a finite set of independent elements of a space
-- See https://en.wikipedia.org/wiki/Free_module
newtype IndependentSet m = IndependentSet (Set m)
    deriving(Eq, Ord, Generic, Data, Typeable, Membership, Discrete)
    deriving(Formattable,Show) via (Set m)
instance Newtype (IndependentSet m)
type instance Individual (IndependentSet m) = m

-- | Represents a finite set of elements which constitute a basis
-- within the context of some space
-- See https://en.wikipedia.org/wiki/Free_module
newtype BasisSet m = BasisSet (Set m)
    deriving(Eq, Ord, Generic, Data, Typeable, Membership, Discrete)
    deriving(Formattable, Show) via (Set m)
instance Newtype (BasisSet m)
type instance Individual (BasisSet m) = m
