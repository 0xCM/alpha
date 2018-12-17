-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators.Logical
(    
    Disjunctive(..), Conjunctive(..), Invertive(..), Logical(..),
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,  

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element
import Alpha.Canonical.Operators.Functions

class Disjunctive a where
    (||)::a -> a -> Bool
    infixr 2 ||

class Conjunctive a where
    (&&)::a -> a -> Bool    
    infixr 3 &&

class Invertive a where
    not::a -> Bool

class (Disjunctive a, Conjunctive a, Invertive a) => Logical a where

-- | Synonym for predicate that saturates with 1 argument
type UnaryPredicate a = UnaryFunc a Bool

-- | Synonym for predicate that saturates with 2 (homogenous) arguments
type BinaryPredicate a = BinaryFunc a a Bool

-- | Synonym for predicate that saturates with 3 (homogenous) arguments
type TernaryPredicate a = TernaryFunc a a a Bool
        
-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a = r | r -> a where
    Predicate 1 a = UnaryPredicate a
    Predicate 2 a = BinaryPredicate a
    Predicate 3 a = TernaryPredicate a


instance Disjunctive Bool where
    (||) = or'    
    {-# INLINE (||) #-}

instance Conjunctive Bool where
    (&&) = and'
    {-# INLINE (&&) #-}

instance Invertive Bool where
    not = not'
    {-# INLINE not #-}

instance Logical Bool    
                    