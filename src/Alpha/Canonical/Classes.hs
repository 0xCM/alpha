-----------------------------------------------------------------------------
-- | Fundamental constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Alpha.Canonical.Classes 
(
    Appendable(..),
    Assembly(..),
    Accumulator(..),
    Boolean(..),
    Computable(..),
    Concatenable(..),
    Chunkable(..),
    Collapsible(..),
    Convertible(..),
    Decomposable(..),
    Dimensional(..),
    Formattable(..),
    Faceted(..),
    FromText(..),
    Indexed(..),
    IndexedChoice(..),
    Jailbreak(..),
    Membership(..),
    Length(..),
    Packable(..),
    Proxy(..),
    ToLines(..),
    Reifiable(..),
    Reversible(..),
    OrderedEnum(..),
    Transposable(..),
    Tupled(..),
    ToString(..),
    Prependable(..),
    FiniteIntegral,
    Partitioner(..),
    Zippable(..)
)
where
import qualified Data.Text as T
import GHC.TypeLits(KnownNat,Nat(..))
import Data.Vector(Vector)
import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import GHC.TypeLits
import qualified Data.List as List

type FiniteIntegral n = (Integral n, FiniteBits n)

-- Synonym for combined Ord and Enum constraints
type OrderedEnum a = (Enum a, Ord a)    

-- Characterizes types from which tuples can be constructed    
class KnownNat n => Tupled n a b where
    -- | Forms a tuple from the source value
    tuple::a -> b

-- | Characterizes a type parameterized by some type 'a'  that is decomposable into a sequence a-values
class Decomposable a b where
    decompose::a -> [b]

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

class Length a where    
    length::forall b. (Num b) => a -> b
    
-- | Characterizes a type that manifests the concept
-- of an invertible reversion    
class Reversible a b | a -> b, b -> a  where
    reverse::a -> b

-- | Characterizes a structure that supports invertible construction/destruction operations
class Assembly a b | a -> b, b -> a where
    disassemble::a -> [b]
    
    assemble::[b] -> a

-- | Characterizes a pair whose terms can be related via an append operation
class Appendable a b where
    type Appended a b
    append::a -> b -> Appended a b

-- | Characterizes a pair whose terms can be related via an prepend operation    
class Prependable a b where
    type Prepended a b
    prepend::a -> b -> Prepended a b
    
-- / Characterizes a pair whose terms can be related via associative concat operations
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
class Boolean a where
    bool::a -> Bool    

-- | Characterizes a value that can be materialized from 'Text'
class FromText a where
    -- | Materializes an 'a'-value from text
    fromText::Text -> a

class Membership s where
    type Member s
    members::s -> Set (Member s) 
        
-- | Characterizes a finite container or other type that contains elements
-- that can be separated into groups of possibly different sizes
class Chunkable a where
    chunk::Int -> a -> [a]
    
-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a

instance Jailbreak Maybe a where
    escape x = fromJust x
    
instance Length a => Measurable 1 a where
    measure = length
        
-- Removes a layer of structure 
-- In the case of a monoid, 'reduce' reduces to 'fold', pun intended
class Collapsible a where
    type Collapsed a
    collapse::a -> Collapsed a
    
-- Characterizes structures that support a notion of duality such that
-- transpose . transpose = id
-- Canonical examples are vectors and matrices
class Transposable a where
    type Transposed a
    type Transposed a = a

    transpose::a -> Transposed a

-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    

-- | Characterizes a pair of types for which transformations are defined 
-- for respectively "packing"  and "unpacking" type values
class Packable a b where
    -- Encodes an a-value to a b-value
    pack::a -> b
    -- Restores an a-value from a b-value
    unpack::b -> a    
    
-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed s i where
    type Found s i
    lookup::s -> i -> Found s i

    (!)::s -> i -> Found s i
    (!) = lookup
            
infixr 0 !

-- Characterizes a family of singleton types 'a' for which the type's single inhabitant
-- is reifiable
class Reifiable (a::k) r where
    reify::r 
        
class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> T.pack
          
-- Captures the assertion that values of a type a can be categorized by
-- values of an enumerable type c
class (Enum c) => Classifiable a c where
    classify::(a -> c) -> [a] -> [(c,a)]

-- Characterizes a type that represents a finite sequence of mutually disjoint choices
class IndexedChoice a where    
    choiceix::a -> Int

-- Characterizes a type for which a notion of dimensionality 
-- can be defined, e.g., an array, matrix or more generally a tensor
class Dimensional a where
    type Dimension a
    dimension::a -> Dimension a
    
class Partitioner a where
    partition::Int -> [a] -> [[a]]
    partition width = List.takeWhile (not . List.null) . fmap (List.take width) . List.iterate (List.drop width)    

instance Partitioner a

-- Characterizes a composition and pairing of heterogenous values
class Zippable a b c where
    
    -- The left source type
    type LeftZip a b c

    -- The right source type
    type RightZip a b c

    -- The result type
    type Zipped a b c

    -- The combining operator
    type Zipper a b c
    
    zip::Zipper a b c -> LeftZip a b c -> RightZip a b c -> Zipped a b c
        
class Computable a where
    type Computation a

    compute::a -> Computation a

-- | Accumulation over a structure    
class Accumulator a where
    type Accumulation a

    accumulate::a -> Accumulation a
        

--From Wadler's Propositions as Types:
--Disjunction A ∨ B corresponds to a disjoint sum A + B, that
--is, a variant with two alternatives. A proof of the proposition
--A ∨ B consists of either a proof of A or a proof of B, including
--an indication of which of the two has been proved. Similarly, a
--value of type A + B consists of either a value of type A or a
--value of type B, including an indication of whether this is a left
--or right summand.
data Disjunct a b = Disjunct (Either a b)

type a :||: b = Disjunct a b

-- From Wadler's Propositions as Types:
-- Conjunction A & B corresponds to Cartesian product A × B,
-- that is, a record with two fields, also known as a pair. A proof
-- of the proposition A&B consists of a proof of A and a proof of
-- B. Similarly, a value of type A × B consists of a value of type
-- A and a value of type B
data Conjunct a b = Conjunct (a,b)

type a :&: b = Conjunct a b

-- From Wadler's Propositions as Types:
--Implication A => B corresponds to function space A -> B. A
--proof of the proposition A => B consists of a procedure that
--given a proof of A yields a proof of B. Similarly, a value of
--type A -> B consists of a function that when applied to a value
--of type A returns a value of type B.
data Implies a b = Implies (a->b)

type a :-> b = Implies a b