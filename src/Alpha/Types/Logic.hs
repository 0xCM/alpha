-----------------------------------------------------------------------------
-- | Defines constructs related to type-level logic
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Alpha.Types.Logic where

import Alpha.Base
import Data.Kind(Type)  
import Data.Proxy
import qualified GHC.TypeLits as TL
import qualified GHC.Natural as TN
import GHC.Types(Symbol)
import Data.Type.Equality hiding (type (==), apply)
    
type True = 'True
type False = 'False

type family Equal a b :: Bool where
    Equal a a = True
    Equal a b = False    

--From Wadler's Propositions as Types:
--Disjunction A ∨ B corresponds to a disjoint sum A + B, that
--is, a variant with two alternatives. A proof of the proposition
--A ∨ B consists of either a proof of A or a proof of B, including
--an indication of which of the two has been proved. Similarly, a
--value of type A + B consists of either a value of type A or a
--value of type B, including an indication of whether this is a left
--or right summand.
data Disjunct a b = Disjunct (Either a b)

type family Or (a::Bool) (b::Bool) :: Bool where
    Or True True = True
    Or True False = True
    Or False True = True
    Or False False = False

type a :||: b = Or a b

-- or'::Proxy a -> Proxy b -> Proxy (a :||: b)
-- or' Proxy Proxy = Proxy
  
type family And (a::Bool) (b::Bool) :: Bool where
    And True True = True
    And True False = False
    And False True = False
    And False False = False    

-- From Wadler's Propositions as Types:
-- Conjunction A & B corresponds to Cartesian product A × B,
-- that is, a record with two fields, also known as a pair. A proof
-- of the proposition A&B consists of a proof of A and a proof of
-- B. Similarly, a value of type A × B consists of a value of type
-- A and a value of type B
data Conjunct a b = Conjunct (a,b)

type a :&: b = And a b

type family Not (a::Bool) :: Bool where    
    Not True = False
    Not False = True

type family If (a::Bool) (b::k) (c::k) :: k where
    If True b c = b
    If False b c = c    

-- | A label to denote the concept of "true"
data T = T
    deriving(Show)

-- | A label to denote the concept of "false"
data F = F
    deriving(Show)
    
-- | Logical False
type family LFalse a where
    LFalse T = F
    LFalse F = F
    
-- | Logical True    
type family LTrue a where
    LTrue T = T
    LTrue F = F

-- | Logical Negation
type family (:~) a where
    (:~) T = F
    (:~) F = T

-- | Logical And    
type family (:&) a b where
    (:&) F F = F
    (:&) F T = F
    (:&) T F = F
    (:&) T T = T

-- | Logical Or        
type family (:|) a b where
    (:|) F F = F
    (:|) F T = T
    (:|) T F = T
    (:|) T T = T

-- | Logical XOr        
type family (:^) a b where
    (:^) F F = F
    (:^) F T = T
    (:^) T F = T
    (:^) T T = F
    
-- | Implies    
type family (:=>) a b where
    (:=>) F F = T
    (:=>) F T = T
    (:=>) T F = F
    (:=>) T T = T

infixl 5 :=>

-- From Wadler's Propositions as Types:
--Implication A => B corresponds to function space A -> B. A
--proof of the proposition A => B consists of a procedure that
--given a proof of A yields a proof of B. Similarly, a value of
--type A -> B consists of a function that when applied to a value
--of type A returns a value of type B.
data Implies a b = Implies (a->b)

type a :-> b = Implies a b    