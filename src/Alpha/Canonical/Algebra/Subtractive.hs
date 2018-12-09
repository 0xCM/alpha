module Alpha.Canonical.Algebra.Subtractive
(
    
    Delta(..), Subtractive(..), HSubtractive(..), Negatable(..),
    
) where
import Alpha.Base hiding(div)
--import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import Alpha.Native


-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Delta a b

-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::BinaryOperator a

    -- | Infix synonym for 'sub'    
    (-)::BinaryOperator a
    (-) = sub
    {-# INLINE (-) #-}
    infixl 6 -    

-- / Characterizes a pair of type that supports a notion of heterogenious subtraction
class HSubtractive a b where
    -- | Calculates the difference between the first value and the second
    hsub::a -> b -> Delta a b

    -- | Infix synonym for 'hsub'        
    (>-<)::a -> b -> Delta a b
    (>-<) = hsub
    infixl 6 >-<    

-- / Characterizes types for which unary negation may be defined
class Negatable a where
    type Negated a
    type Negated a = a

    -- | Negates the operand
    negate::a -> Negated a

    
-- Subtractive
-------------------------------------------------------------------------------
instance Subtractive Natural where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Integer where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int64 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word64 where 
    sub = sub'
    {-# INLINE sub #-}
instance (Integral a) => Subtractive (Ratio a) where 
    sub = sub'
    {-# INLINE sub #-}    
instance Subtractive Float where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Double where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CFloat where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CDouble where 
    sub = sub'
    {-# INLINE sub #-}

-- Negatable 
-------------------------------------------------------------------------------
instance Negatable Natural where 
    type Negated Natural = Integer 
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance Negatable Integer where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int8 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int16 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int32 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int64 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Word where 
    type Negated Word = Int
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance Negatable Word8 where 
    type Negated Word8 = Int8
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance Negatable Word16 where 
    type Negated Word16 = Int16
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance Negatable Word32 where 
    type Negated Word32 = Int32
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance Negatable Word64 where 
    type Negated Word64 = Int64
    negate x = sub' 0 (fromIntegral x)
    {-# INLINE negate #-}
instance (Integral a) => Negatable (Ratio a) where 
    negate x = sub' 0 x
    {-# INLINE negate #-}    
instance Negatable Float where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Double where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CFloat where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CDouble where 
    negate = negate'
    {-# INLINE negate #-}
