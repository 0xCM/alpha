module Alpha.Canonical.Algebra.Polarity
(    
    Sign(..),     
    Signable(..), 
    Unsignable(..),
    Signed(..),
    Unsigned(..),
    positive, negative, neutral
    
) where
import Alpha.Base
import Data.Ord
import Alpha.Native

-- Classifies unsigned numeric types
class Unsigned a where

-- Classifies signednumeric types    
class Signed a where

-- | Defines the codomain of the sign function
data Sign = Negative | Neutral | Positive
    deriving (Show,Ord,Eq,Enum)

-- Characterizes type for which signs may be computed
-- Alternately, characterizes types whose values may be 
-- partitioned into three disjoint subsets, one called 'Negative'
-- one 'Positive' the other 'Neutral'
class Signable a where
    sign::a -> Sign

class Unsignable a where

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
sign' a | a < 0  = Negative
        | a > 0 = Positive
        | a == 0 = Neutral

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
instance Signable Word where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Word8 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Word16 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Word32 where 
    sign = sign'
    {-# INLINE sign #-}
instance Signable Word64 where 
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

instance Unsigned Natural
instance Unsigned Word
instance Unsigned Word8
instance Unsigned Word16
instance Unsigned Word32
instance Unsigned Word64

instance Signed Integer
instance Signed Int
instance Signed Int8
instance Signed Int16
instance Signed Int32
instance Signed Int64
instance Signed Float
instance Signed Double
instance Signed CFloat
instance Signed CDouble
    
