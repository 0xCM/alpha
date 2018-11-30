-----------------------------------------------------------------------------
-- | Signatures and related functions for common operators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Operators
(
    Arital(..),
    Compositional(..),
    Combinable(..), Combiner(..),
    BinaryOperator, UnaryOperator, TernaryOperator,
    Predicate(..), UnaryPredicate, BinaryPredicate, TernaryPredicate,    
    (<|),(|>), ifelse, left,right, symstr, 
    Function, BinaryFunc, UnaryFunc, TernaryFunc,
    type (~>), Hom, Dom, Cod

    
)
where
import Alpha.Base

-- A synonym for a unary function
type Function a b = a -> b

type family (a ~> b)
type Hom a b = a ~> b

type instance (a ~> b) = Function a b

-- Represents a function domain
type family Dom f :: Type where
    Dom (Function a b) = a
    
-- Represents a function codomain
type family Cod f :: Type where
    Cod (Function a b) = b

type family Composition a c where
    Composition (Function b c) (Function a b) = Function a c

newtype Combiner a b c = Combiner (a -> b -> c)

uncombine::Combiner a b c -> (a -> b -> c)
uncombine (Combiner f) = f

-- Represents a transformation accepting potentially heterogenous types 
-- a and b and producing a value representing their combination
class Combinable a b where
    type Combined a b
    
    combiner::Combiner a b (Combined a b)
        
    -- Combines two heterogenous values into one
    combine::a -> b -> Combined a b
    combine a b = f a b 
        where (Combiner f)  =  combiner 
    {-# INLINE combine #-}

    -- Infix alias for 'combine'
    (>.<)::a -> b -> Combined a b
    (>.<) = combine        
    {-# INLINE (>.<) #-}

infixr 0 >.<

    
-- Characterizes function composition
class Compositional a b c where
    compose::Hom b c -> Hom a b -> Hom a c
    compose g f = g . f

-- Characterizes type with which an arity is associated
class KnownNat n => Arital n a where
    arity::a -> Natural
    arity _ = fromIntegral (natVal (Proxy @n))
    
-- A synonym for a function that saturates with 1 argument
type UnaryFunc a1 r = a1 -> r

-- Synonym for endomorphism
type EndoFunc a = UnaryFunc a a

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

-- Characterizes a logical predicate that requires 1 argument for saturation
type UnaryPredicate a = UnaryFunc a Bool

-- Characterizes a logical predicate that requires two arguments for saturation
type BinaryPredicate a = BinaryFunc a a Bool

-- Characterizes a logical predicate that requires three arguments for saturation
type TernaryPredicate a = TernaryFunc a a a Bool

-- Characterizes structures with which a binary operator is associated
class Operative a where
    

-- Characterizes structures with which an associative binary operator is associated
class (Operative a) => Associative (id::Symbol) a where
    bop::BinaryOperator a

-- Characterizes structures with which an associative binary operator is associated
class (Operative a) => Commutative (id::Symbol) a where
    cop::BinaryOperator a
    
-- Specifies an arital predicate 
class (KnownNat n, Arital n a) => Predicative n a where
    predicate::Predicate n a
                    
instance Arital 1 (UnaryOperator a)    
instance Arital 2 (BinaryOperator a)
instance Arital 3 (TernaryOperator a)
    
-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a | a -> a where
    Predicate 1 (a,Bool) =     UnaryPredicate a
    Predicate 2 (a,a,Bool) =   BinaryPredicate a
    Predicate 3 (a,a,a,Bool) = TernaryPredicate a

instance Arital 1 (UnaryPredicate a)    
instance Arital 2 (BinaryPredicate a)
instance Arital 3 (TernaryPredicate a)


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

-- | Produces a strong for a symbol
symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy
