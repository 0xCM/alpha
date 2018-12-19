-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Functions.Operators
(    
    O1, O2, O3,
    OpSpec(..),
    Operator(..),
    Commutative(..), 
    Associative(..),
    Identity(..),

    (<|),(|>), scompose, associator,associative, 
    endoply, cartesian, dual, endo, ifelse, left,right, 
    reduce,
    Iterable(..),
    
) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Elementary

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Synonym for unary operator vis-a-vis: 
-- A *ternary operator* is a total function closed over its domain
type O1 a = a -> a

-- | Synonym for binary operator vis-a-vis: 
-- A *binary operator* is a a total function closed over 
-- an homogenous 2-cartesian domain
type O2 a = a -> a -> a

-- | Synonym for ternary operator vis-a-vis: 
-- A *ternary operator* is a total function closed over 
-- its homogenous 3-cartesian domain
type O3 a = a -> a -> a -> a

-- | Defines arity-polymorphic families of operator specifications
data family OpSpec (n::Nat) f :: Type
data instance OpSpec 1 (O1 a) = O1 a
data instance OpSpec 2 (O2 a) = O2 a
data instance OpSpec 3 (O3 a) = O3 a

-- | Defines evaluation and provisioning services for specified operators
class Operator (n::Nat) f (a::Type) where
    type OpArgs n f a
    operator::OpSpec n f
    evaluate::OpArgs n f a -> a 
    

-- | Classifies commutative binary operators
class (Operator 2 f a) => Commutative f a where

-- | Classifies associative binary operators
class (Operator 2 f a) => Associative f a where

-- | Associaties an identty element with an operator
class (Operator 2 f a) => Identity f a where
        identity::a
        
-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: O1 (Element a) -> (Element a) -> a

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
associator::O2 a -> (a, a, a) -> (a,a)
associator o (x,y,z) =  (left,right) where
    xy = o x y 
    yz = o y z
    left =  o xy z
    right = o x yz

-- | Determines whether a binary operator is associative with respect
-- to a test triple    
associative::(Eq a) => O2 a -> (a, a, a) -> Bool
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

reduce::a -> O2 a -> [a] -> a
reduce id op (a:b:tail) =  op (op a b)  (reduce id op tail)
reduce id op (a:[]) = a
reduce id op [] = id

instance Iterable (Stream a) where
    iterate = Stream.iterate    

instance Iterable [a] where
    iterate = List.iterate            
