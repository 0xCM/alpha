module Alpha.Canonical.Algebra.Multiplicative
(
    Multiplicative(..),
    Multiplied(..), 
    Bimultiplicative(..),    

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Native

-- | Represents a family of types that support a notion of (potentially) heterogenous multiplication
-- where a type instance is the multiplication result type
type family Multiplied a b

-- / Characterizes a type that supports a notion of *associative* multiplication    
-- mul a b == mul b a
class Multiplicative a where

    -- | Multiplies the first operand by the second
    mul::BinaryOperator a

    
    -- | Infix synonym for 'mul'
    (*)::BinaryOperator a
    (*) = mul
    {-# INLINE (*) #-}
    infixl 7 *

-- | Characterizes pairs of types that support a notion multiplication
class Bimultiplicative a b where
    -- | Multiplies the first operand by the second
    bimul::a -> b -> Multiplied a b

    -- | Infix synonym for 'hmul'
    (>*<)::a -> b -> Multiplied a b
    (>*<) = bimul
    {-# INLINE (>*<) #-}    
    infixl 7 >*<

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


instance (Ord a, Multiplicative a) =>  Multiplicative (ItemSet a) where
    mul = intersect'
    
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

type instance Multiplied Natural Natural = Natural
type instance Multiplied Integer Integer = Integer
type instance Multiplied Int Int = Int
type instance Multiplied Int8 Int8 = Int8
type instance Multiplied Int16 Int16 = Int16
type instance Multiplied Int32 Int32 = Int32
type instance Multiplied Int64 Int64 = Int64
type instance Multiplied Word Word = Word
type instance Multiplied Word8 Word8 = Word8
type instance Multiplied Word16 Word16 = Word16
type instance Multiplied Word32 Word32 = Word32
type instance Multiplied Word64 Word64 = Word64
type instance Multiplied (Ratio a) (Ratio a) = Ratio a
type instance Multiplied Float Float = Float
type instance Multiplied Double Double = Double
type instance Multiplied CFloat CFloat = CFloat
type instance Multiplied CDouble CDouble = CDouble
    