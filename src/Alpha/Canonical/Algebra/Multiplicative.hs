-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Multiplicative
(
    Multiplicative(..),
    Factorable(..), Factored(..),
    Unital(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Functions

type family Factored a = r | r -> a

-- / Characterizes a type that supports a notion of *associative* multiplication    
-- mul a b == mul b a
class Multiplicative a where

    -- | Multiplies the first operand by the second
    mul::O2 a
    
    -- | Infix synonym for 'mul'
    (*)::O2 a
    (*) = mul
    {-# INLINE (*) #-}
    infixl 7 *

class Unital a where
    -- | Specifies the unique element 1 such that 1*a = a*1 = a forall a
    one::a

class Factorable (a::Type) where
    --- | Factors a supplied value into constituent parts
    factor::a -> Factored a

    unfactor::Factored a -> a
        
-- newtype Multiplication a = Multiplicative (O2 a)

-- type OpK f a = (f ~ Multiplication a, Multiplicative a, Unital a)

-- data instance OpSpec 2 (Multiplication a) 
--     = Multiplication (O2 a)

-- instance OpK f a => Operator 2 f a where
--     type OpArgs 2 f a = (a,a)

--     operator = Multiplication mul
--     {-# INLINE operator #-}        

--     evaluate (a1,a2) = f a1 a2 where
--             (Multiplication f) = operator 
--     {-# INLINE evaluate #-}        

-- instance OpK f a =>  Commutative f a
-- instance OpK f a =>  Associative f a
-- instance OpK f a => Identity f a where
--     identity = one
    

-- Unital
-------------------------------------------------------------------------------
instance Unital Natural where 
    one = 1
    {-# INLINE one #-}

instance Unital Integer where 
    one = 1
    {-# INLINE one #-}

instance Unital Int where 
    one = 1
    {-# INLINE one #-}

instance Unital Int8 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int16 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int32 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int64 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word where 
    one = 1
    {-# INLINE one #-}

instance Unital Word8 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word16 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word32 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word64 where 
    one = 1
    {-# INLINE one #-}

instance (Integral a) => Unital (Ratio a) where 
    one = 1
    {-# INLINE one #-}

instance Unital Float where 
    one = 1
    {-# INLINE one #-}

instance Unital Double where 
    one = 1
    {-# INLINE one #-}

instance Unital CFloat where 
    one = 1
    {-# INLINE one #-}

instance Unital CDouble where 
    one = 1
    {-# INLINE one #-}

    
---
type Unital2 a1 a2 = (Unital a1, Unital a2)
type Unital3 a1 a2 a3 = (Unital2 a1 a2, Unital a3)
type Unital4 a1 a2 a3 a4 = (Unital3 a1 a2 a3, Unital a4)
type Unital5 a1 a2 a3 a4 a5 = (Unital4 a1 a2 a3 a4, Unital a5)

instance Unital2 a1 a2 => Unital (a1,a2) where
    one = (one,one)

instance Unital3 a1 a2 a3 => Unital (a1,a2,a3) where
    one = (one,one,one)

instance Unital4 a1 a2 a3 a4 => Unital (a1,a2,a3,a4) where
    one = (one,one,one,one)

instance Unital5 a1 a2 a3 a4 a5 => Unital (a1,a2,a3,a4,a5) where
    one = (one,one,one,one,one)
                    
-- Multiplicative
-------------------------------------------------------------------------------
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

