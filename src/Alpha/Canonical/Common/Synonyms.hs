-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Synonyms
(
    OrdEnum(..),
    SemiOrd(..),
    SymNat(..),
    F0,F1,F2,F3,F4,
    P1,P2,P3,
    O1,O2,O3,
    Tuple2,Tuple3,Tuple4,Tuple5,    
    UniTuple2,UniTuple3,UniTuple4,UniTuple5,
    Semigroup2, Semigroup3,Semigroup4,Semigroup5,
    Monoid2, Monoid3, Monoid4, Monoid5,
) where
import Alpha.Base


-- | Synonym for a function accepting no arguments
type F0 a = a

-- | Synonym for a function that saturates with a single argument
type F1 a1 a2 = a1 -> a2

-- | A function that saturates with two arguments
type F2 a1 a2 a3 = a1 -> a2 -> a3

-- | Synonym for a function that saturates with three arguments
type F3 a1 a2 a3 a4 = a1 -> a2 -> a3 -> a4

-- | Synonym for a function that saturates with four arguments
type F4 a1 a2 a3 a4 a5 = a1 -> a2 -> a3 -> a4 -> a5

-- | Synonym for a predicate that saturates with 1 argument
type P1 a = F1 a Bool

-- | Synonym for a predicate that saturates with 2 (homogenous) arguments
type P2 a = F2 a a Bool

-- | Synonym for a predicate that saturates with 3 (homogenous) arguments
type P3 a = F3 a a a Bool

-- | Synonym for a unary operator vis-a-vis: 
-- A *unary operator* is a total function closed over its domain
type O1 a = F1 a a

-- | Synonym for binary operator vis-a-vis: 
-- A *binary operator* is a a total function closed over 
-- an homogenous 2-cartesian domain
type O2 a = F2 a a a

-- | Synonym for ternary operator vis-a-vis: 
-- A *ternary operator* is a total function closed over 
-- its homogenous 3-cartesian domain
type O3 a = F3 a a a a

-- Joins 'Enum' and 'Ord' constraints
type OrdEnum a = (Enum a, Ord a)    

-- | Joins 'KnownSymbol' and 'KnownNat' constraints
type SymNat s n = (KnownSymbol s, KnownNat n)

-- | Joins 'Ord' and 'Semigroup' constraints
type SemiOrd a = (Semigroup a, Ord a)


-- | Synonym for a heterogenous 2-tuple
type Tuple2 a1 a2 = (a1,a2)

-- | Synonym for a heterogenous 3-tuple
type Tuple3 a1 a2 a3 = (a1,a2,a3)

-- | Synonym for a heterogenous 4-tuple
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)

-- | Synonym for a heterogenous 5-tuple
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)

-- Synonym for a homogenous 2-tuple
type UniTuple2 a = Tuple2 a a

-- Synonym for a homogenous 3-tuple
type UniTuple3 a = Tuple3 a a a

-- Synonym for a homogenous 4-tuple
type UniTuple4 a = Tuple4 a a a a

-- Synonym for a homogenous 5-tuple
type UniTuple5 a = Tuple5 a a a a a

-- Joins two 'Semigroup' constraints to form a single two-parameter 'Semigroup' constraint
type Semigroup2 a1 a2 = (Semigroup a1, Semigroup a2)

-- Joins three Semigroup constraints to form a single three-parameter 'Semigroup' constraint
type Semigroup3 a1 a2 a3 = (Semigroup2 a1 a2, Semigroup a3)

-- Joins four Semigroup constraints to form a single four-parameter 'Semigroup' constraint
type Semigroup4 a1 a2 a3 a4 = (Semigroup3 a1 a2 a3, Semigroup a4)

-- Joins five Semigroup constraints to form a single five-parameter 'Semigroup' constraint
type Semigroup5 a1 a2 a3 a4 a5 = (Semigroup4 a1 a2 a3 a4, Semigroup a5)

-- Joins two 'Monoid' constraints to form a single two-parameter 'Monoid' constraint
type Monoid2 a1 a2 = (Monoid a1, Monoid a2)

-- Joins three 'Monoid' constraints to form a single three-parameter 'Monoid' constraint
type Monoid3 a1 a2 a3 = (Monoid2 a1 a2, Monoid a3)

-- Joins four 'Monoid' constraints to form a single four-parameter 'Monoid' constraint
type Monoid4 a1 a2 a3 a4 = (Monoid3 a1 a2 a3, Monoid a4)

type Monoid5 a1 a2 a3 a4 a5 = (Monoid4 a1 a2 a3 a4, Monoid a5)

