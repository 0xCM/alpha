-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators.Logical
(    
    Disjunctive(..), 
    ExclusivelyDisjunct(..), 
    Conjunctive(..), 
    Invertive(..), 
    Implication(..), 
    Propositional(..),
    Substitutive(..),
    Boolean(..),    
    Predicate(..), 
    P1,P2,P3

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element
import Alpha.Canonical.Operators.Functions

-- | Characterizes a value that can be converted to a 'Bool'
class Boolean a where
    bool::a -> Bool    


-- | Characterizes a type's logical disjunction operator 
class Disjunctive a where
    (||)::a -> a -> Bool
    infixr 2 ||

-- | Characterizes a type's logical conjunction operator 
class Conjunctive a where
    (&&)::a -> a -> Bool    
    infixr 3 &&


-- | The modus ponens of propositional logic
-- See https://en.wikipedia.org/wiki/Modus_ponens    
class Implication a where
    implies::a -> a -> Bool


class Substitutive a where
    iff::a -> a -> Bool

    (<->)::a -> a -> Bool
    (<->) = iff
    infixr 1 <->
    
class Invertive a where
    not::a -> Bool

class ExclusivelyDisjunct a where
    lxor::a -> a -> Bool    
    
class (Disjunctive a, ExclusivelyDisjunct a, Conjunctive a, Invertive a, Implication a, Substitutive a) => Propositional a where

-- | Defines arity-polymorphic families of operators
data family Predicate (n::Nat) f :: Type

-- | Defines capabilities for members of the 'Predicate' family
class Questionable (n::Nat) f (a::Type) where
    -- | Defines the type of the input arguments
    type QuestionArgs n f a
    
    -- | Produces the conformed predicate
    predicate::Predicate n f

    -- | Evaluates the predicate
    answer::QuestionArgs n f a -> Bool 

-- | Synonym for predicate that saturates with 1 argument
type P1 a = a -> Bool

-- | Synonym for predicate that saturates with 2 (homogenous) arguments
type P2 a = a -> a -> Bool

-- | Synonym for predicate that saturates with 3 (homogenous) arguments
type P3 a = a -> a -> a -> Bool

data instance Predicate 1 (P1 a) 
    = UnaryPredicate (P1 a)    

data instance Predicate 2 (P2 a) 
    = BinaryPredicate (P1 a)    

data instance Predicate 3 (P3 a) 
    = TernaryPredicate (P1 a)    

        
instance Disjunctive Bool where
    (||) = or'    
    {-# INLINE (||) #-}

instance Conjunctive Bool where
    (&&) = and'
    {-# INLINE (&&) #-}

instance Invertive Bool where
    not = not'
    {-# INLINE not #-}
    
instance Implication Bool where
    implies True True = True
    implies True False = False
    implies False True = True
    implies False False = True 
    {-# INLINE implies #-}
    
instance ExclusivelyDisjunct Bool where
    lxor True True = False
    lxor True False = True
    lxor False True = True
    lxor False False = False
    {-# INLINE lxor #-}

instance Substitutive Bool where
    iff True True = True
    iff True False = False
    iff False True = False
    iff False False = True
    {-# INLINE iff #-}
    
    
instance Propositional Bool    
                    