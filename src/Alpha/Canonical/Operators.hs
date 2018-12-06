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
    type (~>), Hom, 

    Dom(..), Cod(..), Morphism(..), Morphic(..), 
    SymbolPair(..),
    LeftScalar(..), RightScalar(..), Scalar(..),
    Flippable(..),

    endoply, cartesian, dual, endo

    
)
where
import Alpha.Base
import qualified GHC.Base as GB
import qualified Data.Map as Map


type SymbolPair s t = (KnownSymbol s, KnownSymbol t)

-- A synonym for a unary function
type Function a b = a -> b

type family (a ~> b)
type Hom a b = a ~> b

type instance (a ~> b) = Function a b

type family Composition a c where
    Composition (Function b c) (Function a b) = Function a c

-- Defines a function a -> b
data Morphism a b 
    = RuleMorphism (a -> b)    -- ^ | Specifies a morphism via a mapping rule
    | PairMorphism (Map a b)  -- ^ | Specifies a morphism via an explicit list of ordered pairs

type family Dom f 

type family Cod f     

type instance Dom (Morphism a b) = a
type instance Cod (Morphism a b) = b

-- Characterizes a function f
class Morphic f  where    
    -- The characterized function expressed via standard form
    morphism::f -> Morphism (Dom f) (Cod f)

    fx::f -> Dom f -> Cod f
    fx f x = undefined --(eval' morphism) x
    
        where
            eval'::Morphism (Dom f) (Cod f) -> Dom f -> Cod f
            eval' (RuleMorphism g) = undefined
            eval' (PairMorphism pairs) = undefined
         

-- Note that the category of left modules over a ring R is isomorphic to
-- the category or right modules over the opposite ring R^op

class LeftScalar k a where
    type LeftScaled k a

    scaleL::k -> a -> LeftScaled k a

    (*.)::k -> a -> LeftScaled k a
    (*.) = scaleL

infixl 5 *.

class RightScalar a k where
    type RightScaled a k

    scaleR::a -> k -> RightScaled a k

    (.*)::a -> k -> RightScaled a k
    (.*) = scaleR

infixl 5 .*

class (LeftScalar k a, RightScalar a k) => Scalar k a where

newtype Combiner a b c = Combiner (a -> b -> c)


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
        
class Flippable a where
    type Flipped a    
    flip::a -> Flipped a
    
-- Generalizes arity-specific predicates
type family Predicate (n::Nat)  a | a -> a where
    Predicate 1 (a,Bool) =     UnaryPredicate a
    Predicate 2 (a,a,Bool) =   BinaryPredicate a
    Predicate 3 (a,a,a,Bool) = TernaryPredicate a

uncombine::Combiner a b c -> (a -> b -> c)
uncombine (Combiner f) = f

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

instance (Ord a, Ord b) => Flippable (Map a b) where
    type Flipped (Map a b) = Map b a
    flip m = Map.toList m |> fmap (\(y,z) -> (z,y)) |> Map.fromList
  
instance Flippable (a -> b -> c) where
    type Flipped (a -> b -> c) = b -> a -> c
    flip = GB.flip    
            