-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Canonical.Relations.Operators
(    
    Operator(..), operation,
    BinaryOperator(..), O1, O2, O3,
    UnaryOperator(..), 

    Commutative(..), 
    Associative(..),
    Identity(..),
    Inverter(..),

    scompose, associator,associative, 
    endoply, cartesian, dual, endo, left,right, 
    reduce,
    Iterable(..),

    
    StructureOperator(..)
    
) where
import Alpha.Canonical.Elementary
import Alpha.Canonical.Relations.Functions

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

-- Characterizes an operator of arbitrary arity and domain
class Operator f where
    type Operand f
    operator::f
    

-- | An operation is an operator with known domain and arity
newtype Operation n a f = Operation f

type instance Dom (Operation n a f) = UniProduct n a
type instance Cod (Operation n a f) = a
type instance Arity (Operation n a f) = n

class UnaryOperator (f::Type) where
    ueval::f -> Operand f -> Operand f

-- | Defines evaluation and provisioning services for specified operators
class (Operator f) => BinaryOperator (f::Type) where
    evaluate::f -> (Operand f, Operand f) -> Operand f    

-- | Classifies commutative binary operators
class (BinaryOperator f) => Commutative f where

-- | Classifies associative binary operators
class (BinaryOperator f) => Associative f where

-- | Associaties an identity element with a binary operator
class (BinaryOperator f) => Identity f where
    identity::Operand f        

class (UnaryOperator f) => Inverter f where
    invert::Operand f -> Operand f    

class (BinaryOperator f, Structure a, Operand f ~ Individual a) => StructureOperator f a where
    operate::Individual a -> Individual a -> Individual a
    operate x y = evaluate op (x,y) where
        op = operator::f
    
            
-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: O1 (Element a) -> (Element a) -> a

    
operation::forall n a f. (KnownNat n, Operator f) => f ->  Operation n a f
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


