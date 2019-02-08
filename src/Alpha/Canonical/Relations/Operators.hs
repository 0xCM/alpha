-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Relations.Operators
(    
    module X,
    Operator(..), operation,

    Commutative(..), 
    Associative(..), 
    BinaryOperator(..),
    UnaryOperator(..),

    scompose, associator,associative, 
    endoply, cartesian, endo, left,right, 
) where
import Alpha.Canonical.Relations.Common
import Alpha.Canonical.Relations.Functions as X

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Characterizes an operator defined in a structured context
class KnownNat n => Operator n f a where            
    operator::Operation n f a
    evaluate::UniTuple n a -> a

-- | Characterizes a unary operator    
class UnaryOperator a where
    o1::O1 a    
    
-- | Characterizes a binary operator
class BinaryOperator a where
    o2::O2 a

class Commutative a where

-- | Characterizes a type that can produce an associative binary operator
class Associative a where
    

-- | An operation is an operator with known domain and arity
newtype Operation n f a = Operation f
    deriving (Generic)
instance Newtype f =>  Newtype (Operation n f a)


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
        


instance Iterable [a] where
    iterate = List.iterate            

instance Commutative Integer where 
instance Commutative Int where 
instance Commutative Int8 where 
instance Commutative Int16 where 
instance Commutative Int32 where 
instance Commutative Int64 where     
instance Commutative Natural where 
instance Commutative Word where 
instance Commutative Word8 where 
instance Commutative Word16 where 
instance Commutative Word32 where 
instance Commutative Word64 where             
instance (Integral a) => Commutative (Ratio a)
instance Commutative Float
instance Commutative Double
instance Commutative CFloat
instance Commutative CDouble
    