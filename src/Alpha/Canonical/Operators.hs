-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators
(    
    UnaryOperator, BinaryOperator, TernaryOperator,
    
    Faceted(..),Iterable(..),

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

-- | Function synonym  for an operator that saturates with 1 (homogenous)argument
type UnaryOperator a = a -> a

-- | Function synonym for an operator that saturates with 2 (homogenous) arguments
type BinaryOperator a = a -> a -> a

-- | Function synonym for an operator that saturates with 3 (homogenous) arguments
type TernaryOperator a = a -> a -> a -> a

-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: UnaryOperator (Element a) -> (Element a) -> a

class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> Text.pack
    
class Computable a where
    type Computation a

    compute::a -> Computation a
        
    
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
        