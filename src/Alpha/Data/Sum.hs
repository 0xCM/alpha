-----------------------------------------------------------------------------
-- | Defines n-ary Sums
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Sum
(
    Sum1(..), Sum2(..), Sum3(..), Sum4(..), Sum5(..),
    Sum6(..), Sum7(..), Sum8(..), Sum9(..),
    NSum(..), Union(..),
    type (!+!),
    (!+), (+!)
)
where

import Alpha.Base

-- A single-case sum
data Sum1 a1 
    = Sum11 !a1
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 2
data Sum2 a1 a2 
    = Sum21 !a1 | Sum22 !a2
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 3
data Sum3 a1 a2 a3 
    = Sum31 !a1 | Sum32 !a2 | Sum33 !a3
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 4
data Sum4 a1 a2 a3 a4
    = Sum41 !a1 | Sum42 !a2 | Sum43 !a3 | Sum44 !a4
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 5    
data Sum5 a1 a2 a3 a4 a5
    = Sum51 !a1 | Sum52 !a2 | Sum53 !a3 | Sum54 !a4 | Sum55 !a5
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 6    
data Sum6 a1 a2 a3 a4 a5 a6
    = Sum61 !a1 | Sum62 !a2 | Sum63 !a3 | Sum64 !a4 | Sum65 !a5
    | Sum66 !a6
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 7    
data Sum7 a1 a2 a3 a4 a5 a6 a7
    = Sum71 !a1 | Sum72 !a2 | Sum73 !a3 | Sum74 !a4 | Sum75 !a5 
    | Sum76 !a6 | Sum77 !a7
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

-- A disjoint sum of arity 8    
data Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    = Sum81 !a1 | Sum82 !a2 | Sum83 !a3 | Sum84 !a4 | Sum85 !a5
    | Sum86 !a6 | Sum87 !a7 | Sum88 !a8
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

-- A disjoint sum of arity 9    
data Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    = Sum91 !a1 | Sum92 !a2 | Sum93 !a3 | Sum94 !a4 | Sum95 !a5
    | Sum96 !a6 | Sum97 !a7 | Sum98 !a8 | Sum99 !a9
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

type family Union a | a -> a where
    Union (Sum1 a1) = Sum1 a1    
    Union (Sum2 a1 a2) = Sum2 a1 a2
    Union (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    Union (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    Union (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    Union (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6
    Union (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7
    Union (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) = Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    Union (Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9

type a !+! b = Sum2 a b

-- Constructs a left-biased sum
(!+)::a -> a !+! b
(!+) = Sum21

-- Constructs a right-biased sum
(+!)::b -> a !+! b
(+!) = Sum22


-- A disjoint sum of arity n    
type family NSum (n::Nat) a | n -> a where
    NSum 1 (Sum1 a1) = Sum1 a1
    NSum 2 (Sum2 a1 a2) = Sum2 a1 a2
    NSum 3 (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    NSum 4 (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    NSum 5 (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    NSum 6 (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6
    NSum 7 (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7
    NSum 8 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) = Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    NSum 9 (Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9

data DisjointUnion a1 a2 a3 a4 a5 a6 a7 a8 a9
    = DU1 !a1
    | DU2 !a2
    | DU3 !a3
    | DU4 !a4
    | DU5 !a5
    | DU6 !a6
    | DU7 !a7
    | DU8 !a8
    | DU9 !a9
    deriving (Show)
