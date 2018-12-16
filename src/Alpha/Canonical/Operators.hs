-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators
(    
    Func, UnaryFunc, CartesianFunc, BinaryFunc, TernaryFunc,
    Operator(..), UnaryOperator, BinaryOperator, TernaryOperator,
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,  
    Faceted(..),
    Iterable(..),
    Computable(..),


    (<|),(|>), scompose, associator,associative, 
    endoply, cartesian, dual, endo, ifelse, left,right, 

) where
import Alpha.Base
import Alpha.Canonical.Element

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- A synonym for the canonical unary function type
type Func a b = a -> b

-- | Synonym for function that saturates with 1 argument
type UnaryFunc a b = Func a b

-- | Synonym for endomorphism
type EndoFunc a = Func a a

-- | Synonym for function that saturates with 1 cartesian argument
-- that aligns with the following definiion:
-- A **Cartesian function** is any function whose domain consists
-- of 2-tuples, i.e., f:(a,b) -> c 
type CartesianFunc a b c = Func (a,b) c

-- | Synonym for function that saturates with 2 heterogenous arguments
-- that aligns with the follwing definition
-- A **binary function** is a function that accepts two arguments and
-- produces a value. Note that a binary function cannot be cartesian
-- nor conversely.
type BinaryFunc a b c = a -> b -> c

-- | Function synonym that saturates with 3 heterogenous arguments
type TernaryFunc a b c d = a -> b -> c -> d

-- | Function synonym  for an operator that saturates with 1 (homogenous)argument
type UnaryOperator a = a -> a

-- | Function synonym for an operator that saturates with 2 (homogenous) arguments
type BinaryOperator a = a -> a -> a

-- | Function synonym for an operator that saturates with 3 (homogenous) arguments
type TernaryOperator a = a -> a -> a -> a

-- | Synonym for predicate that saturates with 1 argument
type UnaryPredicate a = UnaryFunc a Bool

-- | Synonym for predicate that saturates with 2 (homogenous) arguments
type BinaryPredicate a = BinaryFunc a a Bool

-- | Synonym for predicate that saturates with 3 (homogenous) arguments
type TernaryPredicate a = TernaryFunc a a a Bool
        

type family Operator (n::Nat) a = r | r -> a where
    Operator 1 a = UnaryOperator a
    Operator 2 a = BinaryOperator a
    Operator 3 a = TernaryOperator a

-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a = r | r -> a where
    Predicate 1 a = UnaryPredicate a
    Predicate 2 a = BinaryPredicate a
    Predicate 3 a = TernaryPredicate a

-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: UnaryOperator (Element a) -> (Element a) -> a

class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> Text.pack
    
-- | Characterizes a deferred computation or a computation
-- specification    
class Computable a where
    -- | The type of computed value
    type Computed a

    -- | Effects the computation
    compute::a -> Computed a
            
-- | The forward pipe operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- | The backward pipe operator
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

-- Alias for semigroupoid composition operator
scompose::(Semigroupoid c) => c j k -> c i j -> c i k
scompose = o

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
associative::(Eq a) => BinaryOperator a -> (a, a, a) -> Bool
associative o triple = x == y where
    (x,y) = associator o triple

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

-- Applies an endomorphism
endoply::Endo a -> a -> a
endoply (Endo f) x = f x

-- Constructs a (claimed) endomorphism
endo::(a -> a) -> Endo a
endo f = Endo f    

-- Takes the cartesian product of two lists
cartesian::[a] -> [a] -> [(a,a)]
cartesian xs ys =  [(x,y) | x <- xs, y <- ys]
        
dual::Monoid a => [a] -> Dual a
dual src = fold (fmap  Dual src)

instance Iterable (Stream a) where
    iterate = Stream.iterate    

instance Iterable [a] where
    iterate = List.iterate            