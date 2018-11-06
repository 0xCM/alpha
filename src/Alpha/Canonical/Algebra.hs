{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Algebra
(
    NonEmpty,
    Additive(..),
    Subtractive(..),
    Multiplicative(..),
    --Divisible(..),
    
    Invertible(..),    
    Semigroup, (<>), First(..), Last(..), Min(..), Max(..),
    Monoid, mempty, mappend, mconcat, Dual(..), Endo(..), All(..), Any(..), Sum(..), 
    Semigroupoid(..), compose, 
    Groupoid(..),
    Group(..),

    Ord(..),
    Setoid(..),
    Infimum(..),
    Supremum(..),
    Relation(..),
    Preorder(..),
    Equivalence(..),
    Nullary(..), null,
    Spanned(..),
    Degenerate(..),

    Vector(..), 
    Vectored(..), Covectored(..),
    dual,
    minimal,
    maximal,
    endo,
    endoply,
    cartesian

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
import Data.Ord
import Data.List.NonEmpty hiding(span)
import qualified Data.Vector as V

type family Operator a  where
    Operator (a,a) = a -> a
    Operator (a,a,a) = (a,a) -> a

-- Characterizes a binary operator    
class BinaryOperator a where
    operate::(a,a)-> a

    associator::(a, a, a) -> (a,a)
    associator (x,y,z) =  (left,right) where
        left = operate(operate (x,y), z)
        right = operate(x, operate (y, z))

-- Characterizes a relation on a set s    
class Relation s where
    type Related s
    type Related s = (s,s)

    -- Relation adjudicator
    (~~) ::s -> s -> Bool

-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    sub::a -> a -> a

    (-)::a -> a -> a
    (-) = sub

-- / Characterizes a type that supports a notion of addition      
class Additive a where
    add::a -> a -> a
    
    (+)::a -> a -> a
    (+) = add

-- / Characterizes a type that supports a notion of division
class Divisible a where
    div::a -> a -> a

    (/)::a -> a -> a
    (/) = div

-- / Characterizes a type that supports a notion of multiplication    
class Multiplicative a where
    mul::a -> a -> a
    
    (*)::a -> a -> a
    (*) = mul

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
    equals::a -> a -> Bool
    equals x y = x == y

-- Characterizes relations that are reflexive and transitive
-- See https://en.wikipedia.org/wiki/Preorder
class Relation s => Preorder s where

-- Characterizes preorders that are antisymmetric: a ~~ b and b ~~ a => a = b
-- See https://en.wikipedia.org/wiki/Preorder
class Preorder s => PartialOrder s where

-- Characterizes a partially ordered set
-- See https://en.wikipedia.org/wiki/Partially_ordered_set   
class (Setoid s, PartialOrder s) => Poset s where
    meets::s -> s -> s -> Bool
    joins::s -> s -> s -> Bool

-- Characterizes preorders that are symmetric, and hence 
-- define equivalence relations: a ~= b => b ~= a
class Preorder s => Equivalence r s where
    -- Equivalence relation adjudicator
    (~=) :: s -> s -> Bool
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
class Invertible a b where
    invert::a -> b

class (Ord b) => Spanned a b where
    span::b -> b -> a
    
    (...)::b -> b -> a
    (...) = span


-- | A group is a monoid together with an inversion operator (-)
-- such that for every element g there exists a unique elment -g where g + (-g) = 0
class (Monoid a, Subtractive a, Invertible a a) => Group a where    

data Vector (n::Nat) a 
    = Col (V.Vector a)
    | Row (V.Vector a)
        deriving (Show,Generic,Data,Typeable)    

class Vectored (n::Nat) a b where        
    -- Creates a column vector from coordinate tuples
    col::a -> Vector n b

class Covectored (n::Nat) a b where
    -- Creates a row vector from coordinate tuples
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

