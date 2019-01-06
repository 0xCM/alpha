-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingVia #-}
module Alpha.Canonical.Structures.StructuredSets
(
    SpanningSet(..), spanning,
    IndependentSet(..),
    BasisSet(..), 
    FiniteBasisSet(..)
)
where

import Alpha.Canonical.Algebra

-- | Represents a generating set for m, i.e., every element of
-- m can be expressed as a finite linear combination of elements
-- in the set
-- See https://en.wikipedia.org/wiki/Free_module
newtype SpanningSet m = SpanningSet (Set m)
    deriving(Generic,Associated)
    deriving(Formattable,Show) via (Set m)
instance Newtype (SpanningSet m)

type instance Individual (SpanningSet m) = m

-- | Represents a set of independent elements of 'm'
-- See https://en.wikipedia.org/wiki/Free_module
newtype IndependentSet m = IndependentSet (Set m)
    deriving(Generic,Associated)
    deriving(Formattable,Show) via (Set m)
instance Newtype (IndependentSet m)

type instance Individual (IndependentSet m) = m


newtype FiniteBasisSet m = FiniteBasisSet (Set m)
    deriving(Generic,Associated)
    deriving(Formattable,Show) via (Set m)

instance Newtype (FiniteBasisSet m)

type instance Individual (FiniteBasisSet m) = m

-- | Represents a set of spanning independent elements 
-- of 'm': Every element of  m can be written as
-- a linear combination of elements of m in a unique way
newtype BasisSet m = BasisSet (Set m)
    deriving(Generic,Associated)
    deriving(Formattable,Show) via (Set m)
instance Newtype (BasisSet m)

type instance Individual (BasisSet m) = m


spanning::(Ord a) => [a] -> SpanningSet a
spanning = SpanningSet . fromList

-- basis::(Ord a) => [a] -> BasisSet a
-- basis = BasisSet . fromList

finiteBasis::(Ord a) => [a] -> FiniteBasisSet a
finiteBasis = FiniteBasisSet . fromList
