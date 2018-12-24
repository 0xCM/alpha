-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Hetero
(
    module X,
    Biadditive(..),    
    Multiplied(..), Bimultiplicative(..),    
    Divided(..), Bidivisive(..), 
    Negated(..), Binegatable(..),
    Subtracted(..), Bisubtractive(..),
    Bimodular(..),(/%), (/%%)

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Modular as X

import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval


-- | Represents a family of types that support a notion of (potentially) heterogenous division
-- where a type instance is the type of the result of applying a conforming quotient operator
type family Divided a b     

-- Defines a family of types that represent the result of applying a
-- (potentially) heterogeneous negation operation
type family Negated a

-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Subtracted a b

-- | Represents a family of types that support a notion of (potentially) heterogenous multiplication
-- where a type instance is the multiplication result type
type family Multiplied a b

-- | Characterizes pairs of types that support a notion addition and
-- such addition need not be commutative so, in general,
-- hadd a + b != b + a
class Biadditive a b where
    -- | Adds the first operand with the second
    biadd::a -> b -> Summed a b

    -- | Infix synonym for 'hadd'
    (>+<)::a -> b -> Summed a b
    (>+<) = biadd
    {-# INLINE (>+<) #-}
    infixl 6 >+<

-- | Characterizes pairs of types that support a notion of division
class Bidivisive a b where
    -- | Divides the first value by the second        
    bidiv::a -> b -> Divided a b

    -- | Infix synonym for 'hdiv'
    (>/<)::a -> b -> Divided a b
    (>/<) = bidiv
    {-# INLINE (>/<) #-}        
    infixl 8 >/<


-- / Characterizes types for which unary negation is defined
class Binegatable a where
    -- | Negates the operand    
    binegate::a -> Negated a

-- / Characterizes a pair of type that supports a notion of heterogenious subtraction
class Bisubtractive a b where
    -- | Calculates the difference between the first value and the second
    bisub::a -> b -> Subtracted a b

    -- | Infix synonym for 'hsub'        
    (>-<)::a -> b -> Subtracted a b
    (>-<) = bisub
    infixl 6 >-<    

-- | Characterizes pairs of types that support a notion multiplication
class Bimultiplicative a b where
    -- | Multiplies the first operand by the second
    bimul::a -> b -> Multiplied a b

    -- | Infix synonym for 'hmul'
    (>*<)::a -> b -> Multiplied a b
    (>*<) = bimul
    {-# INLINE (>*<) #-}    
    infixl 7 >*<

class Bimodular a b where
    -- | Calculates the remainder of dividing the first operand by the second
    bimod::a -> b -> Modulo a b

    -- | Infix synonym for 'bimod'
    (>%<)::a -> b -> Modulo a b
    (>%<) = bimod
    {-# INLINE (>%<) #-}
    infix 8 >%<
    
(/%)::(Integral n) => n -> n -> (n,n)
(/%) = quotRem'
{-# INLINE (/%) #-}
infixl 5 /%

(/%%)::(Integral n) => n -> n -> (n,n)
(/%%) = divMod'
{-# INLINE (/%%) #-}
infixl 5 /%%
    
type instance Negated Natural = Integer
type instance Negated Word = Int
type instance Negated Word8 = Int8
type instance Negated Word16 = Int16
type instance Negated Word32 = Int32
type instance Negated Word64 = Int64

type UniSum a = Summed a a
type instance Summed (Tuple2 a1 a2) (Tuple2 a1 a2) = Tuple2 (UniSum a1) (UniSum a2)
type instance Summed (Tuple3 a1 a2 a3) (Tuple3 a1 a2 a3) = Tuple3 (UniSum a1) (UniSum a2) (UniSum a3)
type instance Summed (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 a4) = Tuple4 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4)
type instance Summed (Tuple5 a1 a2 a3 a4 a5) (Tuple5 a1 a2 a3 a4 a5) = Tuple5 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4) (UniSum a5)
    