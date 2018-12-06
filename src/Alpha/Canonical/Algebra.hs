{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Algebra
(    
    Sign(..),     
    Signable(..),    
    Invertible(..),    
    Additive(..), 
    HAdditive(..),
    Multiplicative(..), 
    HMultiplicative(..), 
    Subtractive(..),
    Unital(..),    
    Divisible(..),
    AbelianSemigroup(..),
    Semigroupoid(..),
    Groupoid(..),
    Group(..), 
    AbelianGroup(..),
    Semiring(..),
    Ring(..),
    Setoid(..),
    Nullary(..), 
    LeftModule(..), 
    RightModule(..),
    associator, commutator, reduce, minimal, maximal,
    positive, negative,
    alt, null
)
where
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
import qualified Data.List as List

-- Characterizes structures with which an associative binary operator is associated
class Associative a where

-- Characterizes structures with which a commutative binary operator is associated
class Commutative a where

-- Characterizes a structure with which two operators are associated:
--  The distributor (*) and codistributor (+)  are binary operators such that
-- a1 * (a2 + a3) = a1*a2 + a1*a3 (left distributive)
-- (a1 + a2) * a3 = a1*a3 + a2*a3 (right distributive)
class Distributive a where
    
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
instance (Additive a) => Commutative a

-- | Characterizes pairs of types that support a notion multiplication
class HAdditive a b where
    type HSum a b
    
    hadd::a -> b -> HSum a b

    (>+<)::a -> b -> HSum a b
    (>+<) = hadd
    {-# INLINE (>+<) #-}
    
infixl 6 >+<

-- / Characterizes a type that supports a notion of multiplication    
class Multiplicative a where
    -- | Multiplies the first value by the second
    mul::BinaryOperator a

    -- | Infix synonym for 'mul'
    (*)::BinaryOperator a
    (*) = mul
    {-# INLINE (*) #-}

infixl 7 *

instance (Multiplicative a) => Associative a
instance (Additive a, Multiplicative a) => Distributive a

-- | Characterizes pairs of types that support a notion multiplication
class HMultiplicative a b where
    type HProduct a b
    
    hmul::a -> b -> HProduct a b

    (>*<)::a -> b -> HProduct a b
    (>*<) = hmul
    {-# INLINE (>*<) #-}
    
infixl 7 >*<

-- instance (Multiplicative a) => HMultiplicative a a where
--     type HProduct a a = a
--     hmul = mul

-- / Characterizes a type that supports a notion of division
class Divisible a where
    -- | Divides the first value by the second
    div::BinaryOperator a

    -- | Infix synonym for 'div'
    (/)::BinaryOperator a
    (/) = div
    {-# INLINE (/) #-}

infixl 7 /
    
-- class Structured (a::Type) where
--     type Element a 
--     type Element a = a

-- Tests whether a value is equal to the canonical zero
null::(Eq a, Nullary a)=>a -> Bool
null a = a == zero

-- Characterizes types that are inhabited by a canonical 0/empty value    
-- Note that there is no intent to link nullary and degenerate values
-- although they will at times coincide
class Nullary a where
    -- Specifies the canonical 0 for an element relative to a structure
    zero::a
    
-- Characterizes types or structures that are inhabited by the concept of singularity
class Unital (a::Type) where
    -- Specifies the canonical 1 for an element relative to a structure
    one::a
    
-- / Characterizes a setoid where the required equivalence relation is 
-- interpreteted as equality
-- See https://en.wikipedia.org/wiki/Setoid
class (Eq a) => Setoid a where
    equals::BinaryPredicate a
    equals x y = x == y

-- | Characterizes types whose values are closed under inversion with respect to a multiplicative operator
class Invertible a where
    invert::a -> a

class (Unital a, Invertible a, Monoid a) => Group a where    

class (Semigroup a, Additive a, Subtractive a) => AbelianSemigroup a where    

-- | A group for which the related commutator is always satisfied
class (Group a, Additive a, Subtractive a, Nullary a) => AbelianGroup a where

-- | Almost A ring; elements are not required though to have an additive inverse
class (Additive a, Multiplicative a, Unital a, Nullary a, Monoid a ) => Semiring a where

-- | A ring (with identity)
-- See https://en.wikipedia.org/wiki/Ring_(mathematics)     
class (Group a, Additive a, Subtractive a, Nullary a, Unital a, Multiplicative a, Unital a) 
    => Ring a where

-- | A left module over a ring r
class (Ring r, AbelianGroup g, LeftScalar r g) => LeftModule r g where
    
-- | A right module over a ring r
class (Ring r, AbelianGroup g, RightScalar g r) => RightModule g r where
        
        
data Sign = Negative | Neutral | Positive
    deriving (Show,Ord,Eq)

-- Characterizes type for which signs may be computed
-- Alternately, characterizes types whose values may be 
-- partitioned into three disjoint subsets, one called 'Negative'
-- one 'Positive' the other 'Neutral'
class Signable a where
    sign::a -> Sign
                    
reduce::Semigroup a => [a] -> a
reduce (x:xs) = sconcat (x :| xs)

minimal::(Ord a, Semigroup a) => [a] -> Min a
minimal (x:xs) = sconcat (fmap Min (x :| xs) ) 

maximal::(Ord a, Semigroup a) => [a] -> Max a
maximal (x:xs) = sconcat (fmap Max (x :| xs) ) 
    
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

-- Produces a 'Sign' of neutral polarity
neutral::Sign
neutral = Neutral

-- Alias for semigroupoid composition operator
compose::(Semigroupoid c) => c j k -> c i j -> c i k
compose = o

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
