-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Common
(
    Summed(..), 
    Modulo(..), 
    Divided(..), 
    Negated(..), 
    Subtracted(..), 
    Multiplied(..), 
) where

-- | Represents a family of types that support a notion of (potentially) heterogenous division
-- where a type instance is the type of the result of applying a conforming quotient operator
type family Divided a b     

-- Defines a family of types that represent the result of applying a
-- (potentially) heterogeneous negation operation
type family Negated a

-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Subtracted a b

-- | Represents a family of types that support a notion of (potentially) heterogenous multiplication
-- where a type instance is the multiplication result type
type family Multiplied a b

-- Represents a family of type pairs that support a notion of the first 
-- type 'mod' the second type. Intended to represent to the result of the 
-- modulus operation on integers, a paritioning of a set by a subset or
-- more generally, quotient groups and similar
type family Modulo a b

-- | Represents a family of types that support a notion of (potentially) heterogenous addition
-- where a type instance is the addition result type
type family Summed a b
