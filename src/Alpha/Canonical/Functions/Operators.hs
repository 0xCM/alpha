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
    Operator(..),
    BinaryOperator(..), O1, O2, O3,
    UnaryOperator(..), 

    Commutative(..), 
    Associative(..),
    Identity(..),
    Invertible(..),

    (<|),(|>), scompose, associator,associative, 
    endoply, cartesian, dual, endo, ifelse, left,right, 
    reduce,
    Iterable(..),

    Addition(..), addition,
    Subtraction(..), subtraction,
    Negation(..), negation,
    Multiplication(..), multiplication,
    EuclideanDivision(..), euDivision,
    FloatingDivision(..), flDivision,
    Reciprocation(..), reciprocation,

    
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


class Operator f where
    type Operand f
    operator::f

-- | Defines evaluation and provisioning services for specified operators
class (Operator f) => BinaryOperator (f::Type) where
    evaluate::f -> (Operand f, Operand f) -> Operand f

    
class UnaryOperator (f::Type) where
    ueval::f -> Operand f -> Operand f
    

-- | Classifies commutative binary operators
class (BinaryOperator f) => Commutative f where

-- | Classifies associative binary operators
class (BinaryOperator f) => Associative f where

-- | Associaties an identity element with an operator
class (BinaryOperator f) => Identity f where
    identity::Operand f
        
class (BinaryOperator f) => Invertible f where
    invert::Operand f -> Operand f
        
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


-- Addition
-------------------------------------------------------------------------------

-- | Represents a addition operator
newtype Addition a = Addition (O2 a)    
    deriving(Generic)
instance Newtype (Addition a)

instance (Num a) => Commutative (Addition a)
instance (Num a) => Associative (Addition a)
instance (Num a) => Identity (Addition a) where
    identity = 0

-- | Produces the canonical addition operator
addition::(Num a) => Addition a
addition = Addition add'
    
instance (Num a) => Operator (Addition a) where
    type Operand (Addition a) = a
    operator = addition
    {-# INLINE operator #-}

instance (Num a) => BinaryOperator (Addition a) where
    evaluate (Addition f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}


-- Subtraction
-------------------------------------------------------------------------------

-- | Represents a subtraction operator
newtype Subtraction a = Subtraction (O2 a)    
    deriving(Generic)
instance Newtype (Subtraction a)

instance (Num a) => Associative (Subtraction a)

-- | Produces the canonical subtraction operator
subtraction::(Num a) => Subtraction a
subtraction = Subtraction sub'

instance (Num a) => Operator (Subtraction a) where
    type Operand (Subtraction a) = a
    operator = subtraction
    {-# INLINE operator #-}

instance (Num a) => BinaryOperator (Subtraction a) where
    evaluate (Subtraction f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}

-- Negation
-------------------------------------------------------------------------------

-- | Represents a negation operator
newtype Negation a = Negation (O1 a)    
    deriving(Generic)
instance Newtype (Negation a)

-- | Produces the canonical negation operator
negation::(Num a) => Negation a
negation = Negation negate'

instance (Num a) => Operator (Negation a) where
    type Operand (Negation a) = a    
    operator = negation


instance (Num a) => UnaryOperator (Negation a) where

    ueval (Negation f) a = f a
    {-# INLINE ueval #-}

-- Multiplication
-------------------------------------------------------------------------------

-- | Represents a multiplication operator
newtype Multiplication a = Multiplication (O2 a)    
    deriving(Generic)
instance Newtype (Multiplication a)

-- | Produces the canonical multiplication operator
multiplication::(Num a) => Multiplication a
multiplication = Multiplication mul'

instance (Num a) => Commutative (Multiplication a)
instance (Num a) => Associative (Multiplication a)
instance (Num a) => Identity (Multiplication a) where
    identity = 1

instance (Num a) => Operator (Multiplication a) where
    type Operand (Multiplication a) = a

    operator = multiplication
    {-# INLINE operator #-}


instance (Num a) => BinaryOperator (Multiplication a) where

    evaluate (Multiplication f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}

-- Euclidean Division
-------------------------------------------------------------------------------

-- | Represents a Euclidean division operator
newtype EuclideanDivision a = EuclideanDivision (O2 a)    
    deriving(Generic)
instance Newtype (EuclideanDivision a)

-- | Produces the canonical Euclidean division operator
euDivision::(Integral a) => EuclideanDivision a
euDivision = EuclideanDivision div'

instance (Integral a) => Operator (EuclideanDivision a) where
    type Operand (EuclideanDivision a) = a

    operator = euDivision
    {-# INLINE operator #-}


instance (Integral a) => BinaryOperator (EuclideanDivision a) where
    evaluate (EuclideanDivision f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}

    
-- Floating Division
-------------------------------------------------------------------------------

-- | Represents a floating division operator
newtype FloatingDivision a = FloatingDivision (O2 a)    
    deriving(Generic)
instance Newtype (FloatingDivision a)


-- | Produces the canonical floating division operator    
flDivision::(Floating a) => FloatingDivision a
flDivision = FloatingDivision $ diva

instance (Floating a) => Operator (FloatingDivision a) where
    type Operand (FloatingDivision a) = a

    operator = flDivision
    {-# INLINE operator #-}

instance (Floating a) => BinaryOperator (FloatingDivision a) where
    evaluate (FloatingDivision f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}


-- Reciprocation
-------------------------------------------------------------------------------

-- | Represents a Reciprocation operator

newtype Reciprocation a = Reciprocation (O1 a)
    deriving(Generic)
instance Newtype (Reciprocation a)


-- | Produces the canonical Reciprocation operator
reciprocation::(Fractional a) => Reciprocation a
reciprocation = Reciprocation recip

instance (Fractional a) => Operator (Reciprocation a) where
    type Operand (Reciprocation a) = a

    operator = reciprocation
    {-# INLINE operator #-}

instance (Fractional a) => UnaryOperator (Reciprocation a) where

    ueval (Reciprocation f) a = f a
    {-# INLINE ueval #-}
