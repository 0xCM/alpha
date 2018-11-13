{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Algebra
(    
    NonEmpty,
    Additive(..),
    Subtractive(..),
    Multiplicative(..),
    Unital(..),
    Invertible(..),    
    Semigroupoid(..),
    Groupoid(..),
    Group(..), Abelian(..),
    Ring(..),
    Ord,
    Setoid(..),
    Infimum(..),
    Supremum(..),
    Nullary(..), null,
    Spanned(..),
    Degenerate(..),
    Vector(..), 
    Vectored(..), Covectored(..),
    SignedIntegral(..),UnsignedIntegral(..),
    Monoidal(..),
    dual, minimal, maximal, endo, endoply, cartesian
)
where
import GHC.Base(NonEmpty)
import Data.Functor.Product
import Data.Function
import Data.Foldable hiding(null)
import GHC.Show(Show)
import GHC.TypeLits
import GHC.Num(Num,Natural)
import GHC.Real(Integral)
import GHC.Generics
import Data.Semigroupoid
import qualified Prelude as P
import Data.Ord(Ord)
import Data.List.NonEmpty hiding(span)
import qualified Data.Vector as V

import Alpha.Data.Base
import Alpha.Canonical.Operators
import qualified Data.Monoid as Monoid

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

-- Characterizes types that are inhabited by the concept of singularity
class Unital a where
    one::a
    
-- / Characterizes a setoid where the required equivalence relation is 
-- interpreteted as equality
-- See https://en.wikipedia.org/wiki/Setoid
class (Eq a) => Setoid a where
    equals::BinaryPredicate a
    equals x y = x == y
    
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

-- A finitely presented permutation: A bijection from [1..n] 
data Permutation a = Permutation (Map Natural a) (Map a Natural)

class (Unital a, Nullary a, Semigroup a, Monoid a) => Monoidal a

-- | A group is a monoid together with an inversion operator (-)
-- such that for every element g there exists a unique elment -g where g + (-g) = 0
class (Monoid a, Subtractive a, Invertible a a) => Group a where    

class (Group a, Additive a) => Abelian a where
    
class (Abelian a, Multiplicative a, Unital a) => Ring a where
    
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
    
data Polarity = Polarity{signed::Bool}    


-- Identifies signed integral types
class (Integral i) => SignedIntegral i where


    -- Identifies usigned integral types    
class (Integral i) => UnsignedIntegral i where

    

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

alt::Monoid a => f a -> Monoid.Alt f a
alt = Monoid.Alt
