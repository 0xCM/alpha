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
    module X,
    Reflexive(..),
    Transitive(..),
    Symmetric(..),
    Antisymmetric(..),
    Asymmetric(..),
    Preordering(..),
    Preordered(..),
    Equivalence(..),
    Setoid(..),    
    TotalOrd(..),
    StrictPartialOrder(..),
    PartialOrder(..),
    PartiallyOrdered(..),
    TotallyOrdered(..),
    OrdNum(..),
    Pairs(..),
    Minimal(..), 
    Maximal(..),  
    Infimal(..), 
    Supremal(..),
    Comparer(..), 
    LT(..), 
    GT(..), 
    LTEQ(..), 
    GTEQ(..), 
    Comparable(..),   
    Quotient(..),
    Equation(..),
    Inversion(..),

) where
import Alpha.Canonical.Relations.Common    
import Alpha.Canonical.Relations.Logical as X
import Alpha.Canonical.Relations.Operators as X
import qualified Numeric.Interval as Interval
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Data.List as List

type family Quotient a

--type instance Extremum (Interval a) = a
type instance Individual (Interval a) = a

-- | Synonym for default comparison predicate
type Comparer a = a -> a -> Bool

-- | Defines a collection of ordered pairs    
newtype Pairs a b = Pairs [(a,b)]
    deriving (Show,Eq,Ord,Generic)
instance Newtype (Pairs a b)

-- | Represents a relation between two expressions/values
-- of the form a := b of (potentially) different types
newtype Equation a b = Equation (a,b)
    deriving (Eq, Ord, Generic, Data, Typeable)     

-- | Captures an inversion relation between two values
-- of the same type. What *inversion* means is context 
-- dependent.
newtype Inversion a = Inversion (a, a)    
    deriving (Eq, Ord, Generic, Data, Typeable)     
    
newtype Reflexion a = Reflexion (a,a)
    deriving(Eq,Generic,Ord)

-- Characterizes an reflexive relation: a ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relational a => Reflexive a where
    -- | Establishes a reflexive relation between two points
    reflex::a -> Reflexion a

-- | Captures a transitive relation
newtype Trans a = Trans (Relation a, Relation a)    
    deriving(Eq,Generic,Ord)

-- Characterizes a transitive relation: a ~ b && b ~ c => a ~ c
-- See https://en.wikipedia.org/wiki/Transitive_relation
class Relational a => Transitive a where
    -- | Establishes a transitive relation between two elements
    trans::Relation a -> Relation a -> Trans a
    trans ab bc = Trans(ab,bc)

-- | Captures an a symmetric relation between two values
newtype Symmetry a = Symmetry (a,a)    
    deriving(Eq,Generic,Ord)

-- Characterizes an symmetric relation: a ~ b <=> b ~ a
-- See https://en.wikipedia.org/wiki/Symmetric_relation 
class Relational a => Symmetric a where
    -- | Establishes a symmetric relation between two elements
    symmetry::a -> a -> Symmetry a
    symmetry a b = Symmetry (a,b)

-- | Captures an antisymmetry relation between two values
newtype Antisymmetry a = Antisymmetry (a,a)    
    deriving(Eq,Generic,Ord)
    
-- Characterizes an antisymmetric relation: a ~ b && b ~ a => a = b
-- Or, alternately, an antisymmetric relation precludes both a ~ b and b ~ a
-- from being true
-- See https://en.wikipedia.org/wiki/Antisymmetric_relation 
class Relational a => Antisymmetric a where
    -- | Establishes an antisymmetric relationship between two elements
    antisymmetry::a -> a -> Antisymmetry a
    antisymmetry a b = Antisymmetry(a,b)

-- | Captures an asymmetry relation between two values
newtype Asymmetry a = Asymmetry (a,a)    
    deriving(Eq,Generic,Ord)
    
-- Characterizes an asymmetric relaton: a ~ b => not(b ~ a)    
-- See https://en.wikipedia.org/wiki/Asymmetric_relation
class Relational a => Asymmetric a where
    -- | Establishes an asymmetry relationship between two elements
    asymmetry::a -> a -> Asymmetry a
    asymmetry a b = Asymmetry (a,b)

-- | Captures a preorder relation between two values
newtype Preordering a = Preordering (a,a)    
    deriving(Eq,Generic,Ord)
    
-- | Characterizes a relation that is reflexive and transitive
-- See https://en.wikipedia.org/wiki/Preorder
class (Reflexive a, Transitive a) => Preordered a where
    -- Establishes a preorder relationship between two elements
    preorder::a -> a -> Preordering a
    preorder a b = Preordering (a,b)

-- | Characterizes a relation that is 'Antisymmetric' and 'Transitive'
-- See http://localhost:9000/refs/books/Y2008LOS.pdf#page=21
class (Antisymmetric a, Transitive a)  => StrictPartialOrder a where
    (<::)::P2 a
    
-- | Characterizes a relation that is 'Antisymmetric', 'Transitive', and 'Reflexive'
class (Antisymmetric a, Transitive a, Reflexive a) => PartialOrder a where
    (<:)::P2 a
    (<:) = related
    {-# INLINE (<:) #-}    

    
-- | Characterizes a container whose elements are partially ordered    
class (PartialOrder (Individual a), Container a) => PartiallyOrdered a where

newtype Connex a = Connex (a,a)    
    deriving(Eq,Generic,Ord)

-- Characterizes a relation that relates all elements of a set:
-- For all x and y, x ~ y or y ~ x
-- See https://en.wikipedia.org/wiki/Connex_relation
class Relational a => Connexive a where
    -- | Establish a connex relation between two points
    connex::a -> a -> Connex a

-- | Characterizes a total order relation of the sort that exists
-- amongst the real numbers.
-- See https://en.wikipedia.org/wiki/Total_order     
class (Connexive a, Antisymmetric a, Transitive a) => TotallyOrdered a where
    
-- | Encodes an element of an equivalance relation together with
-- a canonical representative 
newtype Equivalent a = Equivalent (a, [a])
    deriving(Eq,Generic,Ord,Formattable)

instance Formattable a => Show (Equivalent a) where
    show = string . format
    
-- | Defines an equivalance relation via a partition and provides a
-- representation for a collection of 'Equivalent' values    
newtype Partition a = Partition (Map a [a] )
    deriving(Eq,Generic,Ord,Formattable)

instance Formattable a => Show (Partition a) where
    show = string . format

-- Characterizes a relation that is symmetric, reflexive and transitive
-- See https://en.wikipedia.org/wiki/Equivalence_relation
class (Reflexive a, Symmetric a, Transitive a) => Equivalence a where
    -- Determines whether an equivalence relation exists between two elements
    (~=)::P2 a
    (~=) = (~?)
    {-# INLINE (~=) #-}        

    -- | Establishes an equivalence relationship among a collection of elements
    -- together with a canonical representative
    equivilate::a -> [a] -> Equivalent a
    equivilate rep elems = Equivalent (rep, elems)

    -- | Assings an element to its equivalance class in a partition
    classify::a -> Partition a -> Partition a
    default classify::Ord a => a -> Partition a -> Partition a
    classify = classify'

    -- | Divides a list into two parts: those that are related to a given
    -- element and those that are not not. Ironically, the library function
    -- that facilitates this is named 'partition'
    relations::a -> [a] -> ([a],[a])
    relations x candidates = List.partition (\y -> x ~= y) candidates

    
-- | A set together with an equivalence relation
-- See https://en.wikipedia.org/wiki/Setoid
class (Discrete a, Equivalence a) => Setoid a where

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
    supremum::a -> Individual a
            
class LTEQ a where  
    (<=)::Comparer a
    default (<=)::Ord a => Comparer a
    (<=) = (P.<=)
    infix 4 <=
    {-# INLINE (<=) #-}

    -- | Computes the minimum of two values
    min::a -> a -> a
    default min::Ord a => a -> a -> a
    min x y = ifelse (x <= y) x y
    {-# INLINE min #-}    

class LT a where
    (<)::Comparer a
    default (<)::Ord a => Comparer a
    (<) a b = a P.< b
    infix 4 <    
    {-# INLINE (<) #-}

class GT a where    
    (>)::Comparer a
    default (>)::Ord a => Comparer a
    (>) a b = a P.> b
    infix 4 >
    {-# INLINE (>) #-}

class GTEQ a where
    (>=)::Comparer a
    default (>=)::Ord a => Comparer a
    (>=) a b = a P.>= b
    infix 4 >=
    {-# INLINE (>=) #-}

    -- | Computes the maximum of two values
    max::a -> a -> a
    default max::Ord a => a -> a -> a
    max x y = ifelse (x >= y) x y
    {-# INLINE max #-}

class (GTEQ a, GT a, LTEQ a, LT a) => Comparable a where            
    between::P3 a
    between x a b = x >= a && x <= b
    {-# INLINE between #-}

-- Defines a constraint that requires satisfaction of 
-- both 'Ord' and 'Comparable' constraints
type TotalOrd a = (Ord a, Comparable a)

-- | Defines a constraint that requires satisfaction of 
-- both 'TotalOrd' and 'Num' constraints
type OrdNum a = (TotalOrd a, Num a)   

instance Ord a => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance Ord a => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup            

classify'::(Equivalence a, Ord a) => a -> Partition a -> Partition a
classify' elem (Partition part) = Partition modified where
    -- Search for a key that is equivalent to the supplied element
    -- There should be at most 1
    filtered = Map.keys part |> List.filter (\k -> k ~= elem)
    -- Was an equivalent element found ?
    contains = filtered |> List.null
    -- Select the equivalence class representative
    rep = ifelse contains elem ( List.head filtered)        
    -- Bundle the supplied element with other members of the class
    -- or create a new class as appropriate        
    ec = ifelse contains (elem : (part Map.! rep) ) [elem]
    -- Update the partition to include the new element
    modified = ifelse contains 
                        (Map.update (\_ -> Just ec) rep part) 
                        (Map.insert elem [elem] part)
    
-- * LTEQ instances
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

-- *  GTEQ instances
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

-- * LT instances
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

-- * GT instances
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

-- * Comparable instances
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