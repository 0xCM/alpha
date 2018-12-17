-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators.Operative
(    
    Operator(..), UnaryOperator, BinaryOperator, TernaryOperator, BinOp,

    BinaryOperative(..), Commutative(..), Associative(..), O2,

    (<|),(|>), scompose, associator,associative, 
    endoply, cartesian, dual, endo, ifelse, left,right, 
    reduce,
    Iterable(..),


) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: UnaryOperator (Element a) -> (Element a) -> a


-- | Function synonym  for an operator that saturates with 1 (homogenous)argument
type UnaryOperator a = a -> a

-- | Function synonym for an operator that saturates with 2 (homogenous) arguments
--class BinaryOperator a where
type BinOp a = a -> a -> a
data family BinaryOperator a

class BinaryOperative a where
    o2::BinaryOperator a

type O2 a = a -> a -> a

-- | Function synonym for an operator that saturates with 3 (homogenous) arguments
type TernaryOperator a = a -> a -> a -> a


type family Operator (n::Nat) a = r | r -> a where
    Operator 1 a = UnaryOperator a
    Operator 2 a = BinOp a
    Operator 3 a = TernaryOperator a

class Commutative a where

class Associative a where

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
associator::BinOp a -> (a, a, a) -> (a,a)
associator o (x,y,z) =  (left,right) where
    xy = o x y 
    yz = o y z
    left =  o xy z
    right = o x yz

-- | Determines whether a binary operator is associative with respect
-- to a test triple    
associative::(Eq a) => BinOp a -> (a, a, a) -> Bool
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

reduce::a -> BinOp a -> [a] -> a
reduce id op (a:b:tail) =  op (op a b)  (reduce id op tail)
reduce id op (a:[]) = a
reduce id op [] = id

instance Iterable (Stream a) where
    iterate = Stream.iterate    

instance Iterable [a] where
    iterate = List.iterate            
