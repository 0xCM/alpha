-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Logical
(    
    Disjunctive(..), 
    ExclusivelyDisjunct(..), 
    Conjunctive(..), 
    Invertive(..), 
    Implication(..), 
    Propositional(..),
    Biconditional(..),
    Boolean(..),    
    Predicate(..), 
    P1,P2,P3,

    type (==),
    type (&&),
    type (||),
    type (:=>),
    If, Not,
    Disjunct(..), Conjunct(..), Implies(..)

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Elementary
import Alpha.Canonical.Relations.Functions


type True = 'True
type False = 'False

type family a == b :: Bool where
    a == a = True
    a == b = False    
    
infixl 1 ==

data Disjunct a b = Disjunct (Either a b)
data Conjunct a b = Conjunct (a,b)

data Implies a b = Implies (a->b)

type a :-> b = Implies a b    
infixl 5 :->

type family (a::Bool) || (b::Bool) :: Bool where
    True || True = True
    True || False = True
    False || True = True
    False || False = False

infixl 2 ||

type family (a::Bool) && (b::Bool) :: Bool where
    True && True = True
    True && False = False
    False && True = False
    False && False = False    

infixl 3 &&    

type family Not (a::Bool) :: Bool where    
    Not True = False
    Not False = True

type family If (a::Bool) (b::k) (c::k) :: k where
    If True b c = b
    If False b c = c    

type family a :=> b where
    False :=> False = True
    False :=> True = True
    True :=> False = False
    True :=> True = True

infixl 5 :=>
    
-- | Characterizes a value that can be converted to a 'Bool'
class Boolean a where
    bool::a -> Bool    


-- | Characterizes a type's logical disjunction operator 
class Disjunctive a where
    (||)::a -> a -> Bool

-- | Characterizes a type's logical conjunction operator 
class Conjunctive a where
    (&&)::a -> a -> Bool    


-- | The modus ponens of propositional logic
-- See https://en.wikipedia.org/wiki/Modus_ponens    
class Implication a where
    implies::a -> a -> Bool


class Biconditional a where
    iff::a -> a -> Bool

    (<->)::a -> a -> Bool
    (<->) = iff
    infixr 1 <->
    
class Invertive a where
    not::a -> Bool

class ExclusivelyDisjunct a where
    lxor::a -> a -> Bool    
    
class (Disjunctive a, ExclusivelyDisjunct a, Conjunctive a, Invertive a, Implication a, Biconditional a) => Propositional a where

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

instance Biconditional Bool where
    iff True True = True
    iff True False = False
    iff False True = False
    iff False False = True
    {-# INLINE iff #-}
    
    
instance Propositional Bool    
                    