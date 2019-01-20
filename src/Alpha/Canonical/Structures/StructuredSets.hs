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
    FiniteBasisSet(..),

)
where

import Alpha.Canonical.Algebra

-- | Represents a generating set for m, i.e., every element of
-- m can be expressed as a finite combination of elements
-- in the set
-- See 
-- https://en.wikipedia.org/wiki/Generator_(mathematics)
-- https://en.wikipedia.org/wiki/Free_module
newtype GeneratingSet a = GeneratingSet (Set a)
    deriving(Eq, Ord, Generic, Data, Typeable, Associated, Discrete)
    deriving(Formattable,Show) via (Set a)
instance Newtype (GeneratingSet a)

type instance Individual (GeneratingSet a) = a

-- | Represents a set of independent elements of 'm'
-- See https://en.wikipedia.org/wiki/Free_module
newtype IndependentSet m = IndependentSet (Set m)
    deriving(Eq, Ord, Generic, Data, Typeable, Associated, Discrete)
    deriving(Formattable,Show) via (Set m)
instance Newtype (IndependentSet m)

type instance Individual (IndependentSet m) = m


newtype BasisSet m = BasisSet (Set m)
    deriving(Eq, Ord, Generic, Data, Typeable, Associated, Discrete)
    deriving(Formattable, Show) via (Set m)
instance Newtype (BasisSet m)

type instance Individual (BasisSet m) = m

newtype FiniteBasisSet m = FiniteBasisSet (Set m)
    deriving(Eq, Ord, Generic, Data, Typeable, Associated, Discrete)
    deriving(Formattable, Show) via (Set m)
instance Newtype (FiniteBasisSet m)

type instance Individual (FiniteBasisSet m) = m

instance Ord m => Finite (FiniteBasisSet m)


finiteBasis::(Ord a) => [a] -> BasisSet a
finiteBasis = BasisSet . fromList
