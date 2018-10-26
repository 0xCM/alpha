module Alpha.Canonical.Algebra
(
    NonEmpty,
    Additive(..),
    Subtractive(..),
    Multiplicative(..),
    --Divisible(..),
    
    Nullary(..),
    Invertible(..),    
    Semigroup, (<>), First(..), Last(..), Min(..), Max(..),
    Monoid, mempty, mappend, mconcat, Dual(..), Endo(..), All(..), Any(..), Sum(..), Product(..),
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
import Data.Function
import Data.Foldable
import Data.Semigroupoid
import Data.Groupoid(Groupoid(..))
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..))
import Data.Monoid(Dual(..), Endo(..), All(..), Any(..),  Monoid(..), Sum(..), Product(..))
import Data.Ord
import Data.List.NonEmpty

-- | Defines a binary operator over an homogenous domain
newtype BinOp a = BinOp (a -> a -> a) 

class Subtractive a where
    sub::a -> a -> a

    (-)::a -> a -> a
    (-) = sub

class Additive a where
    add::a -> a -> a
    
    (+)::a -> a -> a
    (+) = add

class Divisible a where
    div::a -> a -> a

    (/)::a -> a -> a
    (/) = div

class Multiplicative a where
    mul::a -> a -> a
    
    (*)::a -> a -> a
    (*) = mul
    
class (Eq a) => Setoid a where
    equals::a -> a -> Bool
    equals x y = x == y

-- Characterizes a relation on a set s    
class Relation s where
    type Related s
    type Related s = (s,s)

    -- Relation adjudicator
    (~~) ::s -> s -> Bool

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

-- Characterizes preorders that are symmetric: a ~= b => b ~= a
class Preorder s => Equivalence r s where
    -- Equivalence relation adjudicator
    (~=) :: s -> s -> Bool
    (~=) = (~~)
    
class (Ord a) => Infimum a where
    -- / The greatest lower bound
    infimum::[a] -> a    
    
class (Ord a) => Supremum a where
    -- / The least upper bound
    supremum::[a] -> a

-- | Applied to types that have a canonical unique zero/nullary value
class Nullary a where
    zero::a

-- | Characterizes types whose values can be inverted
class Invertible a b where
    invert::a -> b

-- | A group is a monoid together with an inversion operator (-)
-- such that for every element g there exists a unique elment -g where g + (-g) = 0

class (Monoid a, Subtractive a, Invertible a a) => Group a where    

binapply::BinOp a -> a -> a -> a
binapply (BinOp f) x y = f x y
    
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
    
endo::(Monoid a) => (a -> a) -> Endo a
endo f = Endo f    

endoply::Endo a -> a -> a
endoply (Endo f) x = f x

-- Takes the cartesian product of two lists
cartesian::[a] -> [a] -> [(a,a)]
cartesian xs ys =  [(x,y) | x <- xs, y <- ys]
