-----------------------------------------------------------------------------
-- | Signatures and related functions for common operators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Operators
(
    Operator(..), BinaryOperator, UnaryOperator, TernaryOperator,
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,    
    (<|),(|>), ifelse, left,right, symstr, associator,
    Function, BinaryFunc, UnaryFunc, TernaryFunc,
    type (~>), Hom, Dom, Cod,
    Arital(..),Compositional(..), Invertible(..), 
    Inversion(..), inversion
    

)
where
import Data.Bool
import Data.Either
import Data.Functor
import Data.String
import Data.Proxy
import Data.Kind
import GHC.Natural
import GHC.TypeLits
import Data.Function
import Prelude(fromIntegral)

-- A synonym for a unary function
type Function a b = a -> b

type family (a ~> b)
type Hom a b = a ~> b

type instance (a ~> b) = Function a b

-- Represents a function domain
type family Dom f :: Type where
    Dom (Function a b) = a
    
-- Represents a function codomain
type family Cod f :: Type where
    Cod (Function a b) = b

type family Composition a c where
    Composition (Function b c) (Function a b) = Function a c
    
-- Characterizes function composition
class Compositional a b c where
    compose::Hom b c -> Hom a b -> Hom a c
    compose g f = g . f

-- Characterizes type with which an arity is associated
class KnownNat n => Arital n a where
    arity::a -> Natural
    arity _ = fromIntegral (natVal (Proxy @n))
    
-- A synonym for a function that saturates with 1 argument
type UnaryFunc a1 r = a1 -> r

-- Synonym for endomorphism
type EndoFunc a = UnaryFunc a a

-- A synonym for a function that saturates with 2 arguments
type BinaryFunc a1 a2 r = a1 -> a2 -> r

-- A synonym for a function that saturates with 3 arguments
type TernaryFunc a1 a2 a3 r = a1 -> a2 -> a3 -> r

-- Characterizes an operator that requires 1 argument for saturation
type UnaryOperator a = UnaryFunc a a

-- Characterizes an operator that requires 2 argument for saturation
type BinaryOperator a = BinaryFunc a a a

-- Characterizes an operator that requires 3 argument for saturation
type TernaryOperator a = TernaryFunc a a a a

-- Characterizes a logical predicate that requires 1 argument for saturation
type UnaryPredicate a = UnaryFunc a Bool

-- Characterizes a logical predicate that requires two arguments for saturation
type BinaryPredicate a = BinaryFunc a a Bool

-- Characterizes a logical predicate that requires three arguments for saturation
type TernaryPredicate a = TernaryFunc a a a Bool

-- Generalizes arity-specific operators
type family Operator (n::Nat) a  where
    Operator 1 (a,a) =     UnaryOperator a
    Operator 2 (a,a,a) =   BinaryOperator a
    Operator 3 (a,a,a,a) = TernaryOperator a

-- | Characterizes types whose values can be inverted
-- Note that the operation is not necessarily closed over its
-- domain, but is however total
class Invertible a b where
    invert::a -> b
    
-- Specifies an arital operator
class (KnownNat n, Arital n a) => Operative n a where
    operator::Operator n a

-- Specifies an arital predicate 
class (KnownNat n, Arital n a) => Predicative n a where
    predicate::Predicate n a

-- Captures a pair of functions (f,g) such that f . g = g . f = id
newtype Inversion a b = Inversion (a -> b, b -> a)
    
instance Arital 1 (UnaryOperator a)    
instance Arital 2 (BinaryOperator a)
instance Arital 3 (TernaryOperator a)
    
-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a | a -> a where
    Predicate 1 (a,Bool) =     UnaryPredicate a
    Predicate 2 (a,a,Bool) =   BinaryPredicate a
    Predicate 3 (a,a,a,Bool) = TernaryPredicate a

instance Arital 1 (UnaryPredicate a)    
instance Arital 2 (BinaryPredicate a)
instance Arital 3 (TernaryPredicate a)
        
-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

-- | Constructs a left-valued 'Either'
left :: l -> Either l r
left x = Left x

-- | Constructs a right-valued 'Either'
right :: r -> Either l r
right x = Right x

-- | The forward pipe operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- | The backward pipe operator
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

-- | Produces a strong for a symbol
symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- | Computes both left and right-associative applications
-- If an operator is associtive, these values will coincide
associator::BinaryOperator a -> (a, a, a) -> (a,a)
associator op (x,y,z) =  (left,right) where
    xy = op x y 
    yz = op y z
    left =  op xy z
    right = op x yz

-- | Produces an inversion from complementary inverses    
inversion::(a -> b) -> (b -> a) -> Inversion a b
inversion f g = Inversion(f,g)

instance Invertible (Inversion a b) (Inversion b a) where
    invert (Inversion (f, g)) = Inversion (g, f)