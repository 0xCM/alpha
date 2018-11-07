-----------------------------------------------------------------------------
-- | Fundamental constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Alpha.Canonical.Classes 
(
    Concatenable(..),
    Decomposable(..),
    ToString(..),
    Formattable(..),
    FromText(..),
    Counted(..),
    Jailbreak(..),
    Producer(..),
    Convertible(..),
    Indexed(..),
    Packable(..),
    Length(..),
    Proxy(..),
    ToLines(..),
    ToBool(..),
    Wrapped(..),
    Reifiable(..),
    Chunkable(..),
    Structured(..),
    Faceted(..),
    Flow(..), Coflow(..),
    Collappsible(..),
    Reversible(..),
    Replicator(..),
    Enumerable(..),
    Container(..),
    OrderedEnum(..),
    Assembly(..),
    Unionizable(..),
    Intersectable(..),
    Diffable(..)


)
where
import qualified Data.Text as T
import GHC.TypeLits(KnownNat,Nat(..))
import Data.Vector(Vector)
import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Functors

import qualified Data.List as L

class Sized (n::Nat) c e | e -> c where    

type Router a b = a -> b 

-- Synonym for combined Ord and Enum constraints
type OrderedEnum a = (Enum a, Ord a)    

-- | Characterizes a type parameterized by some type 'a'  that is decomposable into a sequence a-values
class Decomposable a b where
    decompose::a -> [b]

class Structured a b where
    type Construction a b
    type Destructured a b    
    
    destructure::Construction a b -> Destructured a b    

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

class Length a where    
    length::forall b. (Num b) => a -> b
    
instance Length a => Measurable 1 a where
    measure = length

-- | Characterizes a type that manifests the concept
-- of an invertible reversion    
class Reversible a b | a -> b, b -> a  where
    reverse::a -> b


class Enumerable c e | e -> c where 
    -- A source of type 'c' producing elements of type 'e'
    type Source c e
    
    items::Source c e -> [e]

-- | Characterizes a structure that supports invertible construction/destruction operations
class Assembly a b | a->b, b -> a where
    disassemble::a -> [b]
    assemble::[b] -> a
    
-- / Characterizes the concept of a container    
class Container c e | c -> e where
    contains::e -> c  -> Bool

    -- | Constructs a container with exactly one element
    singleton::e -> c

    absent::e -> c -> Bool
    absent item src = not (contains item src)

                
-- / Applicable when values can be joined via a binary function that need not be structure-preserving
class (Measurable n a) => Mixable n a where
    type Mixed n a 
    mix ::a -> a -> Mixed n a

-- / Applicable when values can be concatenated in a "structure-preserving" way
class Concatenable a b where
    type Concatenated a b
    concat :: a -> b -> Concatenated a b

    (+++) :: a -> b -> Concatenated a b
    (+++) x y = concat x y
    infixr 5 +++
    
-- | Characterizes a value that can be converted to a list of 'Text' values
class ToLines a where
    -- | Converts an 'a' value to a list of 'Text' values
    lines::a -> [Text]    
    
-- | Characterizes a value that can be converted to a 'String'
class ToString a where
    -- | Convers an 'a' value to a 'String'
    string::a -> String

-- | Characterizes a value that can be converted to a 'Bool'
class ToBool a where
    bool::a -> Bool    

-- | Characterizes a value that can be materialized from 'Text'
class FromText a where
    -- | Materializes an 'a'-value from text
    fromText::Text -> a
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n

-- | Characterizes a finite container or other type that contains elements
-- that can be partitioned     
class Chunkable a where
    chunk::Int -> a -> [a]
    
-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a

-- Removes a layer of enumerable structure    
-- In the case of a monoid, 'reduce' reduces to 'fold', pun intended
class Collappsible a where
    collapse::[a] -> a
    
-- | Codifies a production relationship between an input value of type 'a' and 
-- an output value of type 'b'
class Producer a b where
    -- | Requires the constuction of a 'b' given an 'a' value
    make::a -> b

-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    
        
class Packable a b where
    pack::a -> b
    unpack::b -> a    
    
class Indexed c e where
    item::c -> Int -> e

    (!)::c -> Int -> e
    (!) = item
            
class Wrapped a b | a -> b where
    unwrap::a -> b

-- A flow from a --> b
class Flow a b where
    flow::a -> (a -> b) -> b
    flow = (|>)

-- A covariant from b --> a
class Coflow a b where
    coflow::(b -> a) -> b -> a
    coflow = (<|)

-- Characterizes a family of singleton types 'a' for which the type's single inhabitant
-- is reifiable
class Reifiable (a::k) r where
    reify::r 
        
class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> T.pack
          
-- Captures the assertion that values of a type a can be partitioned by
-- values of an enumerable type c
class (Enum c) => Classifiable a c where
    classify::(a -> c) -> [a] -> [(c,a)]

class Queryable a where
    take::(Integral i) => i -> [a] -> [a]
    filter::(a -> Bool) -> [a] -> [a]
    
class Replicator a where
    replicate::Int -> a -> [a]
    replicate = L.replicate
    
instance Jailbreak Maybe a where
    escape x = fromJust x

-- Characterizes types for which a meaningful union operation can be specified    
class Unionizable a where
    union::a -> a -> a

-- Characterizes types for which a meaningful intersection operation can be specified        
class Intersectable a where
    intersect::a -> a -> a

-- Characterizes types for which a meaningful delta operation can be specified        
class Diffable a where
    delta::a -> a -> a
    

instance (Eq a) => Unionizable [a] where
    union = L.union
    
instance (Eq a) => Intersectable [a] where
    intersect = L.intersect
    
-- Follows the ideas as presented in Wadler's Propositions as Types

--Disjunction A ∨ B corresponds to a disjoint sum A + B, that
--is, a variant with two alternatives. A proof of the proposition
--A ∨ B consists of either a proof of A or a proof of B, including
--an indication of which of the two has been proved. Similarly, a
--value of type A + B consists of either a value of type A or a
--value of type B, including an indication of whether this is a left
--or right summand.
data Disjunct a b = Disjunct (Either a b)

type a :||: b = Disjunct a b


-- Conjunction A & B corresponds to Cartesian product A × B,
-- that is, a record with two fields, also known as a pair. A proof
-- of the proposition A&B consists of a proof of A and a proof of
-- B. Similarly, a value of type A × B consists of a value of type
-- A and a value of type B
data Conjunct a b = Conjunct (a,b)

type a :&: b = Conjunct a b

--Implication A => B corresponds to function space A -> B. A
--proof of the proposition A => B consists of a procedure that
--given a proof of A yields a proof of B. Similarly, a value of
--type A -> B consists of a function that when applied to a value
--of type A returns a value of type B.
data Implies a b = Implies (a->b)

type a :-> b = Implies a b        