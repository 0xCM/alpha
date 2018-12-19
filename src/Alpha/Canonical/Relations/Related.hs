-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations.Related
(    
    Relation(..),
    Reflexive(..),
    Transitive(..),
    Symmetric(..),
    AntiSymmetric(..),
    Preorder(..),
    Equivalence(..),
    Setoid(..),    
    PartialOrd(..),
    PartialOrder(..),
    Convertible(..),
    Point(..), Pointed(..),
    Related, relate,
    Pairs(..),
    Minimal(..), Maximal(..),  Infimal(..), Supremal(..),
    Infimum(..), Supremum(..), Extremum(..),
    Comparer(..), LT(..), GT(..), LTEQ(..), GTEQ(..), Comparable(..),   

) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Elementary
import Alpha.Canonical.Functions
import qualified Numeric.Interval as Interval
import qualified Prelude as P

type family Infimum a
type family Supremum a    
type family Extremum a

type instance Extremum (Interval a) = a

-- | Synonym for default comparison predicate
type Comparer a = a -> a -> Bool

-- | Encodes that values a and by are related via a relation r
data Related r a b = Related r (a,b)
    deriving (Show,Ord,Eq)

-- | Defines a singleton value    
newtype Point a = Point a
    deriving(Generic,Show)
instance Newtype (Point a)

-- | Defines a collection of ordered pairs    
newtype Pairs a b = Pairs [(a,b)]
    deriving (Show,Eq,Ord,Generic)
instance Newtype (Pairs a b)

-- Characterizes a relation on a set s    
class Relation a where
    -- Relation adjudicator
    related::P2 a

    -- Infix synonym for 'relator'
    (~~)::P2 a
    (~~) = related
    infixl 6 ~~
    
-- Characterizes an reflexive relation: a ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relation a => Reflexive a where

-- Characterizes a transitive relation: a ~ b && b ~ c => a ~ c
-- See https://en.wikipedia.org/wiki/Transitive_relation
class Relation a => Transitive a where

    -- Characterizes an symmetric relation: a ~ b <=> b ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relation a => Symmetric a where

-- Characterizes an antisymmetric relation: a ~ b && b ~ a => a = b
-- See https://en.wikipedia.org/wiki/Antisymmetric_relation 
class Relation a => AntiSymmetric a where

-- Characterizes a relation that is reflexive and transitive
-- See https://en.wikipedia.org/wiki/Preorder
class (Reflexive a, Transitive a) => Preorder a where
    (~<)::P2 a
    (~<) = related

-- Characterizes preorders that are symmetric, and hence 
-- define equivalence relations: a ~= b => b ~= a
class (Reflexive a, Symmetric a, Transitive a) => Equivalence a where
    -- Equivalence relation adjudicator
    (~=)::P2 a
    (~=) = (~~)

-- | A set together with an equivalence relation
-- See https://en.wikipedia.org/wiki/Setoid
class (Set a, Equivalence a) => Setoid a where

class (PartialOrd a, Relation a) =>  PartialOrder a where

    (~<=)::P2 a
    (~<=) = leq
    infix 4 ~<=

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is minimal in A if a <= x for all x in A
class Minimal a where
    -- A minimal element 
    minimum::a -> Element a

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is maximal in A if a >= x for all x in A
class Maximal a where
    maximum::a -> Element a
    
-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimal a where
    -- / The greatest lower bound
    infimum::a -> Extremum a
    
-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremal a where
    -- / The least upper bound
    supremum::a -> Extremum a
            
-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    

class (Ord a) => LTEQ a where
    (<=)::Comparer a
    (<=) a b= a P.<= b
    infix 4 <=
    {-# INLINE (<=) #-}

    -- Computes the minimum of two values    
    min::a -> a -> a
    min x y = ifelse (x <= y) x y
    {-# INLINE min #-}

class (Ord a) => LT a where
    (<)::Comparer a
    (<) a b = a P.< b
    infix 4 <    
    {-# INLINE (<) #-}

class (Ord a) => GT a where    
    (>)::Comparer a
    (>) a b = a P.> b
    infix 4 >
    {-# INLINE (>) #-}

class (Ord a) => GTEQ a where
    (>=)::Comparer a
    (>=) a b = a P.>= b
    infix 4 >=
    {-# INLINE (>=) #-}

    -- Computes the maximum of two values
    max::(Ord a) => a -> a -> a
    max x y = ifelse (x >= y) x y
    {-# INLINE max #-}

class (GTEQ a, GT a, LTEQ a, LT a) => Comparable a where            
    between::P3 a
    between x a b = x >= a || x <= b
    {-# INLINE between #-}
    
-- Classifies a type that has a distinguished value    
class Pointed a where
    point::a -> Point a
    point = Point

relate::r -> (a,b) -> Related r a b
relate  = Related

instance (Ord a, Semigroup a) => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance (Ord a, Semigroup a) => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup            