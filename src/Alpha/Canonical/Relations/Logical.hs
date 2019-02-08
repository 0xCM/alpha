-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Logical
(    
    module X,
    Disjunctive(..), 
    XDisjunctive(..), 
    Conjunctive(..), 
    Invertive(..), 
    Implicative(..), 
    Propositional(..),
    Biconditional(..),
    Boolean(..),    
    Predicate(..), 
    type (&&),
    type (||),
    type (^|),
    type (==>),
    type (<=>),
    Not,


) where
import Alpha.Canonical.Relations.Common
import Alpha.Canonical.Relations.Functions as X

import qualified Data.List as List


type True = 'True
type False = 'False

-- | Captures a disjunction relation
data a || b = Or (a,b)

-- | Captures a conjunction relation
data a && b = And (a,b)

-- | Captures an exclusive or relation
-- See https://en.wikipedia.org/wiki/Exclusive_or
data a ^| b = XOr (a, b)

-- | Captures a material implication relation: if a then b
-- See https://en.wikipedia.org/wiki/Material_conditional
data a ==> b = Implies (a, b)

-- | Captures a bicondition relation: a iff b
-- See https://en.wikipedia.org/wiki/Logical_biconditional
data a <=> b = Iff (a,b)

-- | Captures a logical negation: not a
-- See https://en.wikipedia.org/wiki/Negation
data Not a = Not a

-- | Characterizes types for which a truth value can be assigned
class Boolean a where
    bool::a -> Bool    

-- | Characterizes a type's logical disjunction operator 
class Disjunctive a where
    type Disjunction a
    type Disjunction a = Bool

    (||)::a -> a -> Disjunction a
    infixl 2 ||

-- | Characterizes a type's logical conjunction operator 
class Conjunctive a where
    type Conjunction a
    type Conjunction a = Bool

    (&&)::a -> a -> Conjunction a
    infixl 3 &&    

-- | The modus ponens of propositional logic
-- See https://en.wikipedia.org/wiki/Modus_ponens    
class Implicative a where
    type Implication a
    type Implication a = Bool

    (==>)::a -> a -> Implication a
    infixl 4 ==>

-- | Characterizes a type's biconditional operator
class Biconditional a where
    type Bicondition a
    type Bicondition a = Bool

    (<=>)::a -> a -> Bicondition a
    infixr 1 <=>
    
class Invertive a where
    not::a -> Bool

class XDisjunctive a where
    type XDisjunction a
    type XDisjunction a = Bool

    (^|)::a -> a -> XDisjunction a
    
class (Disjunctive a, XDisjunctive a, Conjunctive a, Invertive a, Implicative a, Biconditional a) => Propositional a where

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
    
instance Implicative Bool where
    True ==> True = True
    True ==> False = False
    False ==> True = True
    False ==> False = True 
    {-# INLINE (==>) #-}
    
instance XDisjunctive Bool where
    True ^| True = False
    True ^| False = True
    False ^| True = True
    False ^| False = False
    {-# INLINE (^|) #-}

instance Biconditional Bool where
    True <=> True = True
    True <=> False = False
    False <=> True = False
    False <=> False = True
    {-# INLINE (<=>) #-}
        
instance Propositional Bool    
                    
instance (Boolean a, Boolean b) => Boolean (a || b) where
    bool (Or (a,b)) = (bool a) || (bool b)

instance Related (a || b) where
    type Relation (a || b) = a || b
    type LeftPart (a || b) = a
    type RightPart (a || b) = b
    relate a b = Or(a,b)
    
instance (Boolean a, Boolean b) => Boolean (a && b) where
    bool (And (a,b)) = (bool a) && (bool b)
    
instance (Boolean a, Boolean b) => Boolean (a ==> b) where
    bool (Implies (a,b)) = (bool a) ==> (bool b)

instance (Boolean a, Boolean b) => Boolean (a <=> b) where
    bool (Iff (a,b)) = (bool a) <=> (bool b)
        
instance (Boolean a, Boolean b) => Boolean (a ^| b) where
    bool (XOr (a,b)) = (bool a) ^| (bool b)
    
    