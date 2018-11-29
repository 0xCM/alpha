{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Algebra
(    
    NonEmpty,
    Sign(..), Signed(..), Unsigned(..),SignedIntegral(..),UnsignedIntegral(..),
    NaturallyPowered(..), IntegrallyPowered(..), ApproximatelyPowered(..),
    Invertible(..), Inverter(..), 
    Additive(..),
    Subtractive(..),
    Negatable(..),
    Multiplicative(..),
    Divisible(..),
    AbelianSemigroup(..),
    Unital(..),
    Semigroupoid(..),
    Groupoid(..),
    Group(..), 
    AbelianGroup(..),
    Ring(..),
    Ord,
    Setoid(..),
    Infimum(..),
    Supremum(..),
    Nullary(..), null,
    Spanned(..),
    Degenerate(..),
    Monoidal(..),
    JoinSemiLattice(..), MeetSemiLattice(..), Lattice(..),
    HMultiplicative(..),
    associator, commutator, inverter,
    positive, negative, sign,
    dual, minimal, maximal, endo, endoply, cartesian, alt
)
where
import Algebra.Lattice(JoinSemiLattice((\/)),MeetSemiLattice((/\)))
import Algebra.Lattice(Lattice(..))   
import Data.Functor.Product
import Data.Function
import Data.Foldable hiding(null)
import Data.Semigroupoid
import Data.Ord(Ord)
import Data.List.NonEmpty hiding(span)

import GHC.Base(NonEmpty)
import GHC.Show(Show)
import GHC.TypeLits
import GHC.Num(Num,Natural)
import GHC.Real(Integral)
import GHC.Generics

import qualified Prelude as P
import qualified Data.Vector as V

import Alpha.Data.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations
import qualified Data.Monoid as Monoid

-- Classifies unsigned numeric types
class (Num a) => Unsigned a where

-- Classifies signednumeric types    
class (Num a) => Signed a where

-- Identifies signed integral types
class (Integral i, Signed i) => SignedIntegral i where

-- Classifies usigned integral types    
class (Integral i, Unsigned i) => UnsignedIntegral i where
    
class NaturallyPowered a where
    pow::(UnsignedIntegral p) => a -> p -> a

    (^)::(UnsignedIntegral p) => a -> p -> a
    (^) = pow
    {-# INLINE (^) #-}

infixr 8 ^

class (Fractional a) => IntegrallyPowered a where
    powi::(Integral p) => a -> p -> a

    (^^)::(Integral p) => a -> p -> a
    (^^) = powi
    {-# INLINE (^^) #-}

infixr 8 ^^

class (Floating a) => ApproximatelyPowered a where
    powa::a -> a -> a

    (**)::a -> a -> a
    (**) = powa
    {-# INLINE (**) #-}

infixr 8 **
        
-- / Characterizes types for which unary negation may be defined
class Negatable a b where
    -- | Negates the operand
    negate::a -> b

-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::BinaryOperator a

    -- | Infix synonym for 'sub'    
    (-)::BinaryOperator a
    (-) = sub
    {-# INLINE (-) #-}

infixl 6 -    

-- / Characterizes a type that supports a notion of addition      
class Additive a where
    add::BinaryOperator a
    
    -- | Infix synonym for 'add'    
    (+)::BinaryOperator a
    (+) = add
    {-# INLINE (+) #-}

infixl 6 +

-- / Characterizes a type that supports a notion of multiplication    
class Multiplicative a where
    -- | Multiplies the first value by the second
    mul::BinaryOperator a

    -- | Infix synonym for 'mul'
    (*)::BinaryOperator a
    (*) = mul
    {-# INLINE (*) #-}

infixl 7 *

-- | Characterizes heterogenous multiplication
class HMultiplicative a b where
    type HProduct a b
    
    hmul::a -> b -> HProduct a b

    (>*<)::a -> b -> HProduct a b
    (>*<) = hmul
    {-# INLINE (>*<) #-}
    
infixl 7 >*<

-- / Characterizes a type that supports a notion of division
class Divisible a where
    -- | Divides the first value by the second
    div::BinaryOperator a

    -- | Infix synonym for 'div'
    (/)::BinaryOperator a
    (/) = div
    {-# INLINE (/) #-}

infixl 7 /

    
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
    
-- | Characterizes a type that contains a relatively contiguous
-- set of values bound by least and greatest values
class (Ord b) => Spanned a b where
    
    -- | Creates a b-value bound by a min and max value
    span::b -> b -> a
    
    -- | The span operator, an infix synonym for 'span'
    (...)::b -> b -> a
    (...) = span

infixl 5 ...
    


-- | Captures a unary operator that produces the inverse of an invertible element
newtype Inverter a = Inverter (UnaryOperator a)

-- | Characterizes types whose values are closed under inversion
class Invertible a where
    invert::a -> a

-- A finitely presented permutation: A bijection from [1..n] 
data Permutation a = Permutation (Map Natural a) (Map a Natural)

class (Unital a, Nullary a, Semigroup a, Monoid a) => Monoidal a where

class Semigroup a => AbelianSemigroup a where    

-- | A group is a monoid together with an inversion operator (-)
-- such that for every element g there exists a unique elment -g where g + (-g) = 0
class (Unital a, Nullary a, Semigroup a, Subtractive a, Invertible a, NaturallyPowered a) => Group a where    

-- | A group for which the related commutator is always satisfied
class (Group a, AbelianSemigroup a) => AbelianGroup a where
    
class (AbelianGroup a, Multiplicative a, Unital a) => Ring a where
        
data Sign = Positive | Negative
    deriving (Show,Ord,Eq)

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

-- Constructs a (claimed) endomorphism
endo::(a -> a) -> Endo a
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

-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
alt::Monoid a => f a -> Monoid.Alt f a
alt = Monoid.Alt

-- Produces a 'Sign' of positive polarity
positive::Sign
positive = Positive

-- Produces a 'Sign' of negative polarity
negative::Sign
negative = Negative

-- Computes the sign of a value
sign::(TotalOrder a, Nullary a) => a -> Sign
sign a = ifelse (a >= zero) positive negative

-- | Produces an interver from a unary operator
inverter::UnaryOperator a -> Inverter a
inverter = Inverter

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Invertible a) => BinaryOperator a -> (a -> a -> a)
commutator o =  \x y ->  o (o (invert x) (invert y)) (o x y) where

-- | Computes both left and right-associative applications
-- If an operator is associtive, these values will coincide
associator::BinaryOperator a -> (a, a, a) -> (a,a)
associator o (x,y,z) =  (left,right) where
    xy = o x y 
    yz = o y z
    left =  o xy z
    right = o x yz

-- | Determines whether a binary operator is associative with respect
-- to a test triple    
associates::(Eq a) => BinaryOperator a -> (a, a, a) -> Bool
associates o triple = x == y where
    (x,y) = associator o triple
