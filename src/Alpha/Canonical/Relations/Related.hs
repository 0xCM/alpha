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
    TotalOrd(..),
    PartialOrder(..),
    TotalOrder(..),
    OrdNum(..),
    Point(..), 
    Pointed(..),
    Pairs(..),
    Minimal(..), 
    Maximal(..),  
    Infimal(..), 
    Supremal(..),
    Infimum(..), 
    Supremum(..), 
    Extremum(..),
    Comparer(..), 
    LT(..), 
    GT(..), 
    LTEQ(..), 
    GTEQ(..), 
    Comparable(..),   
    Quotient(..)

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
type family Quotient a


type instance Extremum (Interval a) = a
type instance Individual (Interval a) = a

-- | Synonym for default comparison predicate
type Comparer a = a -> a -> Bool

-- -- | Defines a singleton value    
-- newtype Point a = Point a
--     deriving(Generic,Show)
-- instance Newtype (Point a)

-- | Specifies the type of a point relative to a set/space
type family Point d

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
            
class (Ord a) => LTEQ a where  
    (<=)::Comparer a
    (<=) = (P.<=)
    infix 4 <=
    {-# INLINE (<=) #-}

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

-- Defines a constraint that requires satisfaction of 
-- both 'Ord' and 'Comparable' constraints
type TotalOrd a = (Ord a, Comparable a)

-- Defines a constraint that requires satisfaction of 
-- both 'TotalOrd' and 'Num' constraints
type OrdNum a = (TotalOrd a, Num a)   

-- Classifies a type that has a distinguished value    
class Pointed a where
    point::a -> Point a    

instance (Ord a) => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance (Ord a) => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup            

-- PartialOrd instances
-------------------------------------------------------------------------------
instance PartialOrd Natural where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Integer where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Int where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Int8 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Int16 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Int32 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Int64 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Word where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Word8 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Word16 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Word32 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Word64 where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance (Integral a, Ord a) => PartialOrd (Ratio a) where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Float where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd Double where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd CFloat where
    leq a b = a P.<= b
    {-# INLINE leq #-}

instance PartialOrd CDouble where
    leq a b = a P.<= b
    {-# INLINE leq #-}

-- LTEQ instances
-------------------------------------------------------------------------------
instance LTEQ Natural
instance LTEQ Integer
instance LTEQ Int
instance LTEQ Int8
instance LTEQ Int16
instance LTEQ Int32
instance LTEQ Int64
instance LTEQ Word
instance LTEQ Word8
instance LTEQ Word16
instance LTEQ Word32
instance LTEQ Word64
instance (Integral a, Ord a) => LTEQ (Ratio a)
instance LTEQ Float
instance LTEQ Double
instance LTEQ CFloat
instance LTEQ CDouble

-- GTEQ instances
-------------------------------------------------------------------------------
instance GTEQ Natural
instance GTEQ Integer
instance GTEQ Int
instance GTEQ Int8
instance GTEQ Int16
instance GTEQ Int32
instance GTEQ Int64
instance GTEQ Word
instance GTEQ Word8
instance GTEQ Word16
instance GTEQ Word32
instance GTEQ Word64
instance (Integral a, Ord a) => GTEQ (Ratio a)
instance GTEQ Float
instance GTEQ Double
instance GTEQ CFloat
instance GTEQ CDouble

-- LT instances
-------------------------------------------------------------------------------
instance LT Natural
instance LT Integer
instance LT Int
instance LT Int8
instance LT Int16
instance LT Int32
instance LT Int64
instance LT Word
instance LT Word8
instance LT Word16
instance LT Word32
instance LT Word64
instance (Integral a, Ord a) => LT (Ratio a)
instance LT Float
instance LT Double
instance LT CFloat
instance LT CDouble

-- GT instances
-------------------------------------------------------------------------------
instance GT Natural
instance GT Integer
instance GT Int
instance GT Int8
instance GT Int16
instance GT Int32
instance GT Int64
instance GT Word
instance GT Word8
instance GT Word16
instance GT Word32
instance GT Word64
instance (Integral a, Ord a) => GT (Ratio a)
instance GT Float
instance GT Double
instance GT CFloat
instance GT CDouble

-- Comparable instances
-------------------------------------------------------------------------------
instance Comparable Natural
instance Comparable Integer
instance Comparable Int
instance Comparable Int8
instance Comparable Int16
instance Comparable Int32
instance Comparable Int64
instance Comparable Word
instance Comparable Word8
instance Comparable Word16
instance Comparable Word32
instance Comparable Word64
instance (Integral a, Ord a) => Comparable (Ratio a)
instance Comparable Float
instance Comparable Double
instance Comparable CFloat
instance Comparable CDouble

