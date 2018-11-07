{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Algebra
(
    module X,
    NonEmpty,
    Additive(..),
    Subtractive(..),
    Multiplicative(..),
    Invertible(..),    
    Semigroup, (<>), First(..), Last(..), Min(..), Max(..),
    Monoid, mempty, mappend, mconcat, Dual(..), Endo(..), All(..), Any(..), Sum(..), 
    Semigroupoid(..), compose, 
    Groupoid(..),
    Group(..),
    Ord,
    Setoid(..),
    Infimum(..),
    Supremum(..),
    Relation(..),
    Preorder(..),
    PartialOrder(..), Poset(..),
    TotalOrder(..), Chain(..),
    Equivalence(..),
    Nullary(..), null,
    Spanned(..),
    Degenerate(..),
    Vector(..), 
    Vectored(..), Covectored(..),
    dual, minimal, maximal, endo, endoply, cartesian, associator


)



where
import GHC.Base(NonEmpty)
import Data.Eq
import Data.Bool
import Data.Functor
import Data.Functor.Product
import Data.Function
import Data.Foldable hiding(null)
import Data.Data
import GHC.Show(Show)
import GHC.TypeLits
import GHC.Generics
import Data.Semigroupoid
import Data.Groupoid(Groupoid(..))
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..))
import Data.Monoid(Dual(..), Endo(..), All(..), Any(..),  Monoid(..), Sum(..),Alt(..))
import qualified Prelude as P
import Data.Ord(Ord)
import Data.List.NonEmpty hiding(span)
import qualified Data.Vector as V

import Alpha.Canonical.Functions as X


-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::BinaryOperator a

    -- | Infix synonym for 'sub'    
    (-)::BinaryOperator a
    (-) = sub

infixl 6 -    

-- / Characterizes a type that supports a notion of addition      
class Additive a where
    add::BinaryOperator a
    
    -- | Infix synonym for 'add'    
    (+)::BinaryOperator a
    (+) = add

infixl 6 +

-- / Characterizes a type that supports a notion of division
class Divisible a where
    -- | Divides the first value by the second
    div::BinaryOperator a

    -- | Infix synonym for 'div'
    (/)::BinaryOperator a
    (/) = div

infixl 7 /

-- / Characterizes a type that supports a notion of multiplication    
class Multiplicative a where
    -- | Multiplies the first value by the second
    mul::BinaryOperator a

    -- | Infix synonym for 'mul'
    (*)::BinaryOperator a
    (*) = mul

infixl 7 *

-- Characterizes types that are inhabited by 'degenerate' values
-- Examples include empty lists, mathematical intervals 
-- that represent a single value, etc. What precicely constitutes a 
-- a degenerate value for a given type is implementation-defined
-- See https://en.wikipedia.org/wiki/Degeneracy_(mathematics)
class Degenerate a where
    -- Test for degeneracy
    degenerate::a -> Bool     

-- Characterizes types that are inhabited by a canonical 0/empty value    
-- Note that there is no intent to link nullary and degenerate values
-- although they will at times coincide
class Nullary a where
    -- Specifies the canonical a-valued 0
    zero::a
    
-- / Characterizes a setoid where the required equivalence relation is 
-- interpreteted as equality
-- See https://en.wikipedia.org/wiki/Setoid
class (Eq a) => Setoid a where
    equals::BinaryPredicate a
    equals x y = x == y

-- Characterizes a relation on a set s    
class Relation a where

    -- Relation adjudicator
    related::BinaryPredicate a

    -- Infix synonym for 'related'
    (~~)::BinaryPredicate a
    (~~) = related

infixl 6 ~~

-- Characterizes relations that are reflexive and transitive
-- See https://en.wikipedia.org/wiki/Preorder
class Relation s => Preorder s where

-- Characterizes preorders that are antisymmetric: a ~~ b and b ~~ a => a = b
-- See https://en.wikipedia.org/wiki/Preorder
class Preorder a => PartialOrder a where
    (~<)::BinaryPredicate a
    (~<) = (~~)

-- Characterizes a total order relation
-- https://en.wikipedia.org/wiki/Total_order    
class Ord a => TotalOrder a where
    (<=)::a -> a -> Bool
    (<=) a b= a P.<= b

    (<)::a -> a -> Bool
    (<) a b = a P.< b

    (>)::a -> a -> Bool
    (>) a b = a P.> b

    (>=)::a -> a -> Bool
    (>=) a b = a P.>= b

infix 4 <=
infix 4 <    
infix 4 >
infix 4 >=    
    

-- Characterizes a partially ordered set
-- See https://en.wikipedia.org/wiki/Partially_ordered_set   
class (Setoid a, PartialOrder a) => Poset a where
    meets::TernaryPredicate a
    joins::TernaryPredicate a

-- Characterizes a totally ordered set, otherwise known as a chain
-- See https://en.wikipedia.org/wiki/Total_order  
class (Setoid a, TotalOrder a) => Chain a where

-- Characterizes preorders that are symmetric, and hence 
-- define equivalence relations: a ~= b => b ~= a
class Preorder a => Equivalence a where
    -- Equivalence relation adjudicator
    (~=)::BinaryPredicate a
    (~=) = (~~)
    
-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimum a b where
    -- / The greatest lower bound
    infimum::a -> b
    
-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremum a b where
    -- / The least upper bound
    supremum::a -> b
    
-- | Characterizes types whose values can be inverted
-- Note that the operation is not necessarily closed over its
-- domain, but is however total
class Invertible a b where
    invert::a -> b

-- | Characterizes a type that contains a relatively contiguous
-- set of values bound by least and greatest values
class (Ord b) => Spanned a b where
    
    -- | Creates a b-value bound by a min and max value
    span::b -> b -> a
    
    -- | The span operator, an infix synonym for 'span'
    (...)::b -> b -> a
    (...) = span

infixl 5 ...

-- | A group is a monoid together with an inversion operator (-)
-- such that for every element g there exists a unique elment -g where g + (-g) = 0
class (Monoid a, Subtractive a, Invertible a a) => Group a where    

-- | Represents a sized vector
data Vector (n::Nat) a 
    = Col (V.Vector a)
    | Row (V.Vector a)
        deriving (Show,Generic,Data,Typeable)    

-- | Characterizes types that can be expressed as a sized vector        
class Vectored (n::Nat) a b where        
    -- Creates a column vector from an a-value
    col::a -> Vector n b

-- | Characterizes types that can be expressed as a sized covector        
class Covectored (n::Nat) a b where
    -- Creates a row vector from an a-value
    row::a -> Vector n b
    
-- Alias for semigroupoid composition operator
compose::(Semigroupoid c) => c j k -> c i j -> c i k
compose = o

reduce::Semigroup a => [a] -> a
reduce (x:xs) = sconcat (x :| xs)
        
dual::Monoid a => [a] -> Dual a
dual src = fold (fmap  Dual src)

minimal::(Ord a, Semigroup a) => [a] -> Min a
minimal (x:xs) = sconcat (fmap Min (x :| xs) ) 

maximal::(Ord a, Semigroup a) => [a] -> Max a
maximal (x:xs) = sconcat (fmap Max (x :| xs) ) 
    
-- Constructs an endomorphism
endo::(Monoid a) => (a -> a) -> Endo a
endo f = Endo f    

-- Applies an endomorphism
endoply::Endo a -> a -> a
endoply (Endo f) x = f x

-- Takes the cartesian product of two lists
cartesian::[a] -> [a] -> [(a,a)]
cartesian xs ys =  [(x,y) | x <- xs, y <- ys]

-- Tests whether a value is equal to the canonical zero
null::(Eq a, Nullary a) => a -> Bool
null a = a == zero

alt::Monoid a => f a -> Alt f a
alt = Alt

-- Computes both left and right-associative applications
-- If an operator is associtive, these values will coincide
associator::BinaryOperator a -> (a, a, a) -> (a,a)
associator op (x,y,z) =  (left,right) where
    xy = op x y 
    yz = op y z
    left =  op xy z
    right = op x yz

