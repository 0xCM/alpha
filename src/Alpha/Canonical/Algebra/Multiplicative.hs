-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Algebra.Multiplicative
(
    module X,
    Multiplicative(..),
    Invertible(..),
    Bimultiplicative(..),    
    Multiplied(..),
    Product(..),
    product,

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Unital as X

type family Multiplied a b

-- | Represents a formal product of an arbitrary
-- number of elements
newtype Product a = Product [a]    

-- / Characterizes a type that supports a notion of *associative* multiplication    
-- mul a b == mul b a
class Multiplicative a where

    -- | Multiplies the first operand by the second
    mul::O2 a
    mul = (*)
    {-# INLINE mul #-}

    -- | Infix synonym for 'mul'
    (*)::O2 a
    (*) = mul
    {-# INLINE (*) #-}
    infixl 7 *

-- | Characterizes pairs of types that support a notion multiplication
class Bimultiplicative a b where
    -- | Multiplies the first operand by the second
    bimul::a -> b -> Multiplied a b
    bimul = (>*<)
    {-# INLINE bimul #-}    

    -- | Infix synonym for 'hmul'
    (>*<)::a -> b -> Multiplied a b
    (>*<) = bimul
    {-# INLINE (>*<) #-}    
    infixl 7 >*<

-- | Characterizes types whose values are closed under 
-- mulitiplicative inversion 
class Invertible a where
    invert::a -> a

-------------------------------------------------------------------------------        
-- * Invertible instances
-------------------------------------------------------------------------------
instance (Integral a) => Invertible (Ratio a) where
    invert = recip
    {-# INLINE invert #-}

instance Invertible Float where 
    invert = recip
    {-# INLINE invert #-}

instance Invertible Double where 
    invert =  recip
    {-# INLINE invert #-}

instance Invertible CFloat where 
    invert = recip
    {-# INLINE invert #-}

instance Invertible CDouble where 
    invert = recip
    {-# INLINE invert #-}    

-- | Constructs a summation
product::[a] -> Product a
product = Product

-- *Computable instances
-------------------------------------------------------------------------------

instance (Unital a, Multiplicative a) => Computable (Product a) where
    type Computed (Product a) = a
    compute (Product items) = reduce one (*) items
                
-- * Multiplicative instances
-------------------------------------------------------------------------------
instance Multiplicative a => Multiplicative (Vector a) where
    v1 * v2 = vmix (*) (vecpair v1 v2)  where

        
instance (Ord a, Multiplicative a) =>  Multiplicative (FiniteSet a) where
    mul x y = intersect x y

instance Multiplicative Natural where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Integer where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Int where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Int8 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Int16 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Int32 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Int64 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Word where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Word8 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Word16 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Word32 where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Word64 where 
    mul = mul'
    {-# INLINE mul #-}

instance (Integral a) => Multiplicative (Ratio a) where 
    mul = mul'
    {-# INLINE mul #-}    

instance Multiplicative Float where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative Double where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative CFloat where 
    mul = mul'
    {-# INLINE mul #-}

instance Multiplicative CDouble where 
    mul = mul'
    {-# INLINE mul #-}

type Multiplicative2 a1 a2 = (Multiplicative a1, Multiplicative a2)
type Multiplicative3 a1 a2 a3 = (Multiplicative2 a1 a2, Multiplicative a3)
type Multiplicative4 a1 a2 a3 a4 = (Multiplicative3 a1 a2 a3, Multiplicative a4)
type Multiplicative5 a1 a2 a3 a4 a5 = (Multiplicative4 a1 a2 a3 a4, Multiplicative a5)

instance Multiplicative2 a1 a2 => Multiplicative (a1,a2) where
    mul (x1,x2) (y1,y2) = (x1*y1,x2*y2)    

instance Multiplicative3 a1 a2 a3 => Multiplicative (a1,a2,a3) where
    mul (x1,x2,x3) (y1,y2,y3) = (x1*y1,x2*y2,x3*y3)

instance Multiplicative4 a1 a2 a3 a4 => Multiplicative (a1,a2,a3,a4) where
    mul (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1*y1,x2*y2,x3*y3,x4*y4)

instance Multiplicative5 a1 a2 a3 a4 a5 => Multiplicative (a1,a2,a3,a4,a5) where
    mul (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1*y1,x2*y2,x3*y3,x4*y4,x5*y5)

