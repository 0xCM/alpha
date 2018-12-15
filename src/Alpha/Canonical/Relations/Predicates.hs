-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Predicates
(
    Predicate(..), 
    UnaryPredicate, BinaryPredicate, TernaryPredicate,  Comparer,
) where

import Alpha.Base
import Alpha.Canonical.Relations.Functions


-- | Synonym for predicate that saturates with 1 argument
type UnaryPredicate a = UnaryFunc a Bool

-- | Synonym for predicate that saturates with 2 (homogenous) arguments
type BinaryPredicate a = BinaryFunc a a Bool

-- | Synonym for predicate that saturates with 3 (homogenous) arguments
type TernaryPredicate a = TernaryFunc a a a Bool
        
-- | Synonym for function that effects heterogenous comparison
type Comparer a b = a -> b -> Bool

-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a | a -> a where
    Predicate 1 (a,Bool) =     UnaryPredicate a
    Predicate 2 (a,a,Bool) =   BinaryPredicate a
    Predicate 3 (a,a,a,Bool) = TernaryPredicate a
