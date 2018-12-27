module Alpha.Canonical.Common.Signage
(
    BoundedIntegral(..),
    Unsignable(..),
    Sign(..),
    UnsignedIntegral(..),
    SignedIntegral(..),
    Signable(..),

    positive,negative,neutral,
    integers,
) where
import Alpha.Canonical.Common.Root
import qualified Data.List as List

-- | Defines the codomain of the sign function
data Sign = Negative | Neutral | Positive
    deriving (Show,Ord,Eq,Enum)
        
-- | Characterizes type for which signs may be computed
-- Alternately, characterizes types whose values may be 
-- partitioned into three disjoint subsets, one called 'Negative'
-- one 'Positive' the other 'Neutral'
class Signable a where
    sign::a -> Sign

-- | Classifies signed integral types    
class (Signable i, Integral i) => SignedIntegral i where

class (Unsignable i, Integral i) => UnsignedIntegral i where

-- | Classifies types with which a sign cannot be associated
class Unsignable a where

class (Bounded a, Integral a) => BoundedIntegral a where    

-- | Enumerates bounded integral values
integers::(BoundedIntegral n) => NonEmpty n
integers = (List.head range) :| (List.tail range)
    where range = [minBound .. maxBound]

-- Produces a 'Sign' of positive polarity
positive::Sign
positive = Positive

-- Produces a 'Sign' of negative polarity
negative::Sign
negative = Negative

-- Produces a 'Sign' of neutral polarity
neutral::Sign
neutral = Neutral

sign'::(Num a,Ord a) => a -> Sign
sign' a | lt' a 0 = Negative
        | gt' a 0 = Positive
        | a == 0 = Neutral

instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64
instance UnsignedIntegral Natural


instance BoundedIntegral Int
instance BoundedIntegral Int8
instance BoundedIntegral Int16
instance BoundedIntegral Int32
instance BoundedIntegral Int64
instance BoundedIntegral Word
instance BoundedIntegral Word8
instance BoundedIntegral Word16
instance BoundedIntegral Word32
instance BoundedIntegral Word64        
    

instance Unsignable Natural
instance Unsignable Word
instance Unsignable Word8
instance Unsignable Word16
instance Unsignable Word32
instance Unsignable Word64

instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64
instance SignedIntegral Integer
        
    
instance Signable Natural where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Integer where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Int where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Int8 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Int16 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Int32 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Int64 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Float where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Double where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable CFloat where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable CDouble where 
    sign = sign'
    {-# INLINE sign #-}
instance (Integral a) => Signable (Ratio a) where
    sign = sign'
    {-# INLINE sign #-}
        