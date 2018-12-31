-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Synonyms
(
    OrdEnum(..),
    F0,F1,F2,F3,F4,
    P1,P2,P3,
    O1,O2,O3,
    Tuple2,Tuple3,Tuple4,Tuple5,    
    UniTuple2,UniTuple3,UniTuple4,UniTuple5
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

-- Synonym for combined Ord and Enum constraints
type OrdEnum a = (Enum a, Ord a)    

type Tuple2 a1 a2 = (a1,a2)
type Tuple3 a1 a2 a3 = (a1,a2,a3)
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)

type UniTuple2 a = Tuple2 a a
type UniTuple3 a = Tuple3 a a a
type UniTuple4 a = Tuple4 a a a a
type UniTuple5 a = Tuple5 a a a a a

