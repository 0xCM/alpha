module Alpha.Canonical.Algebra.Scalar
(    
    LeftScalar(..), RightScalar(..), Scalar(..),
    
) where
import Alpha.Base
import Alpha.Native

-- Note that the category of left modules over a ring R is isomorphic to
-- the category or right modules over the opposite ring R^op

class LeftScalar k a where
    type LeftScaled k a

    -- | Left-multiplication of a by a scalar k
    scaleL::k -> a -> LeftScaled k a

    -- | Left-multiplication of a by a scalar k
    (*.)::k -> a -> LeftScaled k a
    (*.) = scaleL
    infixl 5 *.

class RightScalar a k where
    type RightScaled a k

    -- | Right-multiplication of a by a scalar k    
    scaleR::a -> k -> RightScaled a k

    -- | Right-multiplication of a by a scalar k    
    (.*)::a -> k -> RightScaled a k
    (.*) = scaleR
    infixl 5 .*

class (LeftScalar k a, RightScalar a k) => Scalar k a where

-- LeftScalar 
-------------------------------------------------------------------------------

instance LeftScalar Natural Natural where
    type LeftScaled Natural Natural = Natural 
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Integer Integer where 
    type LeftScaled Integer Integer = Integer
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Int Int where 
    type LeftScaled Int Int = Int
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Int8 Int8 where 
    type LeftScaled Int8 Int8 = Int8
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Int16 Int16 where 
    type LeftScaled Int16 Int16 = Int16
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Int32 Int32 where 
    type LeftScaled Int32 Int32 = Int32
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Int64 Int64 where 
    type LeftScaled Int64 Int64 = Int64
    scaleL = mul'    
    {-# INLINE scaleL #-}

instance LeftScalar Word Word where 
    type LeftScaled Word Word  = Word
    scaleL = mul'    
    {-# INLINE scaleL #-}

instance LeftScalar Word8 Word8 where 
    type LeftScaled Word8 Word8 = Word8
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Word16 Word16 where 
    type LeftScaled Word16 Word16 = Word16
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Word32 Word32 where 
    type LeftScaled Word32 Word32 = Word32    
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Word64 Word64 where 
    type LeftScaled Word64 Word64 = Word64
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Float Float where 
    type LeftScaled Float Float = Float    
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar Double Double where 
    type LeftScaled Double Double = Double
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar CFloat CFloat where 
    type LeftScaled CFloat CFloat = CFloat    
    scaleL = mul'
    {-# INLINE scaleL #-}

instance LeftScalar CDouble CDouble where 
    type LeftScaled CDouble CDouble = CDouble    
    scaleL = mul'
    {-# INLINE scaleL #-}

-- RightScalar 
-------------------------------------------------------------------------------

instance RightScalar Natural Natural where
    type RightScaled Natural Natural = Natural 
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Integer Integer where 
    type RightScaled Integer Integer = Integer
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Int Int where 
    type RightScaled Int Int = Int
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Int8 Int8 where 
    type RightScaled Int8 Int8 = Int8
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Int16 Int16 where 
    type RightScaled Int16 Int16 = Int16
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Int32 Int32 where 
    type RightScaled Int32 Int32 = Int32
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Int64 Int64 where 
    type RightScaled Int64 Int64 = Int64
    scaleR = mul'    
    {-# INLINE scaleR #-}

instance RightScalar Word Word where 
    type RightScaled Word Word  = Word
    scaleR = mul'    
    {-# INLINE scaleR #-}

instance RightScalar Word8 Word8 where 
    type RightScaled Word8 Word8 = Word8
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Word16 Word16 where 
    type RightScaled Word16 Word16 = Word16
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Word32 Word32 where 
    type RightScaled Word32 Word32 = Word32    
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Word64 Word64 where 
    type RightScaled Word64 Word64 = Word64
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Float Float where 
    type RightScaled Float Float = Float    
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar Double Double where 
    type RightScaled Double Double = Double
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar CFloat CFloat where 
    type RightScaled CFloat CFloat = CFloat    
    scaleR = mul'
    {-# INLINE scaleR #-}

instance RightScalar CDouble CDouble where 
    type RightScaled CDouble CDouble = CDouble    
    scaleR = mul'
    {-# INLINE scaleR #-}    
