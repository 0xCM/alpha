-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Relations.Operators
(    
    Operator(..), operation,
    O1, O2, O3,
    Compositer(..),

    Commutative(..), 
    Associative(..), 
    Identity(..), 
    Inverter(..), 
    BinaryOperator(..),
    UnaryOperator(..),

    scompose, associator,associative, 
    endoply, cartesian, dual, endo, left,right, 
    reduce,
    Iterable(..),
    
    
) where
import Alpha.Canonical.Elementary
import Alpha.Canonical.Relations.Functions

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Synonym for unary operator vis-a-vis: 
-- A *unary operator* is a total function closed over its domain
type O1 a = a -> a

-- | Synonym for binary operator vis-a-vis: 
-- A *binary operator* is a a total function closed over 
-- an homogenous 2-cartesian domain
type O2 a = a -> a -> a

-- | Synonym for ternary operator vis-a-vis: 
-- A *ternary operator* is a total function closed over 
-- its homogenous 3-cartesian domain
type O3 a = a -> a -> a -> a

-- | Characterizes a type for which a binary operation is defined
-- What Blythe calls an "internal law of composition"
class Compositer a where
    composite::O2 a

    (~.~)::O2 a
    (~.~) = composite

-- | Characterizes an operator defined in a structured context
class KnownNat n => Operator n f a where            
    operator::Operation n f a
    evaluate::UniTuple n a -> a
        
-- | Characterizes a type that can produce a unary operator
class UnaryOperator f a where
    o1::f -> O1 a    

-- | Characterizes a type that can produce a unary inverter
class UnaryOperator f a => Inverter f a where
    inverter::f -> O1 a
    
-- | Characterizes a type that can produce a binary operator
class BinaryOperator f a where
    o2::f -> O2 a

-- | Characterizes a type that can produce a commutative binary operator
class BinaryOperator f a => Commutative f a where

-- | Characterizes a type that can produce an associative binary operator
class BinaryOperator f a => Associative f a where

class Associative f a => Identity f a where
    identity::a
    
-- | Characterizes a structure with a binary operation
-- See https://en.wikipedia.org/wiki/Magma_(algebra)    
class Compositer a => Magma a where    

-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: O1 (Individual a) -> (Individual a) -> a


-- | An operation is an operator with known domain and arity
newtype Operation n f a = Operation f
    deriving (Generic)
instance Newtype f =>  Newtype (Operation n f a)

type instance Dom (Operation n f a) = UniProduct n a
type instance Cod (Operation n f a) = a
type instance Arity (Operation n f a) = n

operation::forall n f a. (KnownNat n, Operator n f a) => f -> Operation n f a
operation = Operation
                
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

instance Iterable [a] where
    iterate = List.iterate            