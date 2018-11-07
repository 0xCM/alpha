module Alpha.Canonical.Functions
(
    Operator(..), BinaryOperator, UnaryOperator, TernaryOperator,
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,
    ifelse, left,right,map, symstr,
    (<|),(|>),
    BinaryFunc, UnaryFunc,
    Func0, Func1,Func2,Func3,Func4,Func5,Func6,Func7,Func8,Func9

)
where
import Data.Bool
import Data.Either
import Data.Functor
import Data.String
import Data.Proxy
import GHC.TypeLits

-- Characterizes an operator that requires 1 argument for saturation
type UnaryOperator a = a -> a

-- Characterizes an operator that requires 2 argument for saturation
type BinaryOperator a = a -> a -> a

-- Characterizes an operator that requires 3 argument for saturation
type TernaryOperator a = a -> a -> a -> a

-- Generalizes arity-specific operators
type family Operator a  where
    Operator (a,a) =    UnaryOperator a
    Operator (a,a,a) =  BinaryOperator a
    Operator (a,a,a,a) = TernaryOperator a

-- Characterizes a logical predicate that requires 1 argument for saturation
type UnaryPredicate a = a -> Bool

-- Characterizes a logical predicate that requires two arguments for saturation
type BinaryPredicate a = a -> a -> Bool

-- Characterizes a logical predicate that requires three arguments for saturation
type TernaryPredicate a = a -> a -> a -> Bool

-- Generalizes arity-specific predicates
type family Predicate a | a -> a where
    Predicate (a,Bool) = UnaryPredicate a
    Predicate (a,a,Bool) = BinaryPredicate a
    Predicate (a,a,a,Bool) = TernaryPredicate a

-- A synonym for a function that saturates with 1 argument
type UnaryFunc a1 r = a1 -> r

-- A synonym for a function that saturates with 2 arguments
type BinaryFunc a1 a2 r = a1 -> a2 -> r

-- A synonym for a constant function
type Func0 r  = r

-- A synonym for a function that saturates with 1 argument
type Func1 a1 r = a1 -> r

-- A synonym for a function that saturates with 2 arguments
type Func2 a1 a2 r = a1 -> a2 -> r

-- A synonym for a function that saturates with 3 arguments
type Func3 a1 a2 a3 r = a1 -> a2 -> a3 -> r

-- A synonym for a function that saturates with 4 arguments
type Func4 a1 a2 a3 a4 r = a1 -> a2 -> a3 -> a4 -> r

-- A synonym for a function that saturates with 5 arguments
type Func5 a1 a2 a3 a4 a5 r = a1 -> a2 -> a3 -> a4 -> a5 -> r

-- A synonym for a function that saturates with 6 arguments
type Func6 a1 a2 a3 a4 a5 a6 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r

-- A synonym for a function that saturates with 7 arguments
type Func7 a1 a2 a3 a4 a5 a6 a7 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r

-- A synonym for a function that saturates with 8 arguments
type Func8 a1 a2 a3 a4 a5 a6 a7 a8 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r

-- A synonym for a function that saturates with 9 arguments
type Func9 a1 a2 a3 a4 a5 a6 a7 a8 a9 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r

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

map::(Functor f) => (a -> b) -> f a -> f b
map = fmap

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
