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
    Antisymmetric(..),
    Asymmetric(..),
    Preorder(..),
    Equivalence(..),
    Setoid(..),    
    PartialOrd(..),
    OrdPartialOrd(..),
    Point(..), Pointed(..),
    Pairs(..),
    Minimal(..), Maximal(..),  Infimal(..), Supremal(..),
    Infimum(..), Supremum(..), Extremum(..),
    Comparer(..), LT(..), GT(..), LTEQ(..), GTEQ(..), Comparable(..),   

) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Elementary
import Alpha.Canonical.Relations.Logical
import Alpha.Canonical.Relations.Operators
import qualified Numeric.Interval as Interval
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Data.List as List

type family Infimum a
type family Supremum a    
type family Extremum a

type instance Extremum (Interval a) = a
type instance Individual (Interval a) = a

-- | Synonym for default comparison predicate
type Comparer a = a -> a -> Bool

-- | Defines a singleton value    
newtype Point a = Point a
    deriving(Generic,Show)
instance Newtype (Point a)

-- | Defines a collection of ordered pairs    
newtype Pairs a b = Pairs [(a,b)]
    deriving (Show,Eq,Ord,Generic)
instance Newtype (Pairs a b)

-- Characterizes a binary relation on a set s    
class (Eq a) =>  Relation a where
    type Related a
    type Related a = (a,a)

    -- | Determines whether two points are related
    related::P2 a

    -- | Establishes a relation between two points
    relate::a -> a -> Related a

    -- | Infix synonym for 'related'
    (~~)::P2 a
    (~~) = related
    infixl 6 ~~
    

-- Characterizes an reflexive relation: a ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relation a => Reflexive a where
    type Reflexed a
    type Reflexed a = (a,a)

    -- | Establishes a reflexive relation between two points
    reflex::a -> Reflexed a

-- Characterizes a transitive relation: a ~ b && b ~ c => a ~ c
-- See https://en.wikipedia.org/wiki/Transitive_relation
class Relation a => Transitive a where
    type Trans a
    type Trans a = (a,a)

    -- | Establishes a transitive relation between two elements
    trans::a -> a -> Trans a

    -- Characterizes an symmetric relation: a ~ b <=> b ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relation a => Symmetric a where
    type Symmetry a
    type Symmetry a = (a,a)

    -- | Establishes a symmetric relation between two elements
    symmetry::a -> a -> Symmetry a
    
-- Characterizes an antisymmetric relation: a ~ b && b ~ a => a = b
-- Or, alternately, an antisymmetric relation precludes both a ~ b and b ~ a
-- from being true
-- See https://en.wikipedia.org/wiki/Antisymmetric_relation 
class Relation a => Antisymmetric a where
    type Antisymmetry a
    type Antisymmetry a = (a,a)

    -- | Establishes an antisymmetric relationship between two elements
    antisymmetry::a -> a -> Antisymmetry a


-- Characterizes an asymmetric relaton: a ~ b => not(b ~ a)    
-- See https://en.wikipedia.org/wiki/Asymmetric_relation
class Relation a => Asymmetric a where
    type Asymmetry a
    type Asymmetry a = (a,a)

    -- | Establishes an asymmetry relationship between two elements
    asymmetry::a -> a -> Asymmetry a
    
-- Characterizes a relation that is reflexive and transitive
-- See https://en.wikipedia.org/wiki/Preorder
class (Reflexive a, Transitive a) => Preorder a where
    type Preordered a
    type Preordered a = (a,a)

    -- Establishes a preorder relationship between two elements
    preorder::a -> a -> Preordered a

-- Characterizes a relation that is 'Antisymmetric' and 'Reflexive' and 'Transitive'
class (Antisymmetric a, Reflexive a, Transitive a) => PartialOrder a where
    type PartiallyOrdered a
    type PartiallyOrdered a = (a,a)

    (<:)::P2 a
    (<:) = related
    {-# INLINE (<:) #-}    

-- Characterizes a relation that relates all elements of a set:
-- For all x and y, x ~ y or y ~ x
-- See https://en.wikipedia.org/wiki/Connex_relation
class Relation a => Connexive a where
    type Connex a
    type Connex a = (a,a)    

    -- | Establish a connex relation between two points
    connex::a -> a -> Connex a

-- | Characterizes a total order relation of the sort that exists
-- amongst the real numbers.
-- See https://en.wikipedia.org/wiki/Total_order     
class (Connexive a, Antisymmetric a, Transitive a) => TotalOrder a where
    type TotallyOrdered a
    type TotallyOrdered a = (a,a)

    -- | Establish a total order relation between two points
    totalord::a -> a -> TotallyOrdered a

-- Characterizes a relation that is symmetric, reflexive and transitive
-- See https://en.wikipedia.org/wiki/Equivalence_relation
class (Reflexive a, Symmetric a, Transitive a) => Equivalence a where
    type Equivalent a
    type Equivalent a = (a,a)

    -- Determines whether an equivalence relation exists between two elements
    (~=)::P2 a
    (~=) = related
    {-# INLINE (~=) #-}        

    -- | Establishes an equivalence relationship between two points
    eqivilate::a -> a -> Equivalent a


    partition::[a] -> Partition a
    partition _ = undefined

    -- | Divides a list into two parts: those that are related to a given
    -- element and those that are not not. Ironically, the library function
    -- that facilitates this is named 'partition'
    relations::a -> [a] -> ([a],[a])
    relations x candidates = List.partition (\y -> x ~= y) candidates
    
newtype Partition a = Partition (Map a [a] )




class (PartialOrd a, Ord a) => OrdPartialOrd a where
    (<=)::Comparer a
    infix 4 <=
       
    
-- | A set together with an equivalence relation
-- See https://en.wikipedia.org/wiki/Setoid
class Equivalence a => Setoid a where


-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is minimal in A if a <= x for all x in A
class Minimal a where
    -- A minimal element 
    minimum::a -> Individual a

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is maximal in A if a >= x for all x in A
class Maximal a where
    maximum::a -> Individual a
    
-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimal a where
    -- / The greatest lower bound
    infimum::a -> Individual a
    
-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremal a where
    -- / The least upper bound
    supremum::a -> Extremum a
            

class (OrdPartialOrd a) => LTEQ a where    -- Computes the minimum of two values    
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

-- relate::r -> (a,b) -> Related r a b
-- relate  = Related

instance (Ord a, Semigroup a) => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance (Ord a, Semigroup a) => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup            