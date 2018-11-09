-----------------------------------------------------------------------------
-- | Signatures and related functions for common operators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators
(
    Operator(..), BinaryOperator, UnaryOperator, TernaryOperator,
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,    
    (<|),(|>), ifelse, left,right, symstr, associator,
    BinaryFunc, UnaryFunc, TernaryFunc,
    type (~>), Hom, Dom, Cod
    

)
where
import Data.Bool
import Data.Either
import Data.Functor
import Data.String
import Data.Proxy
import Data.Kind
import GHC.TypeLits

type family (x ~> y)

type family Dom f :: Type where
    Dom (x->y) = x
    
type family Cod f :: Type where
    Cod (x->y) = y

type Hom x y = x ~> y

type instance (x ~> y) = x -> y

-- A synonym for a function that saturates with 1 argument
type UnaryFunc a1 r = a1 -> r

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

-- Generalizes arity-specific operators
type family Operator a  where
    Operator (a,a) =    UnaryOperator a
    Operator (a,a,a) =  BinaryOperator a
    Operator (a,a,a,a) = TernaryOperator a

-- Characterizes a logical predicate that requires 1 argument for saturation
type UnaryPredicate a = UnaryFunc a Bool

-- Characterizes a logical predicate that requires two arguments for saturation
type BinaryPredicate a = BinaryFunc a a Bool

-- Characterizes a logical predicate that requires three arguments for saturation
type TernaryPredicate a = TernaryFunc a a a Bool

-- Generalizes arity-specific predicates
type family Predicate a | a -> a where
    Predicate (a,Bool) = UnaryPredicate a
    Predicate (a,a,Bool) = BinaryPredicate a
    Predicate (a,a,a,Bool) = TernaryPredicate a


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

symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy

-- Computes both left and right-associative applications
-- If an operator is associtive, these values will coincide
associator::BinaryOperator a -> (a, a, a) -> (a,a)
associator op (x,y,z) =  (left,right) where
    xy = op x y 
    yz = op y z
    left =  op xy z
    right = op x yz
