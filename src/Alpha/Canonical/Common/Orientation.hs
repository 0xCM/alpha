-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Common.Orientation
(
    module X,
    Unsignable(..),
    Sign(..),
    Signable(..),
    Unbounded(..),
    Oriented(..),
    Orientation(..),
    LeftOrient(..),
    RightOrient(..),
    positive,negative,neutral,leftward,rightward,unbounded,

    BoundedIntegral(..),
    UnsignedIntegral(..),
    SignedIntegral(..),

) where
import Alpha.Canonical.Common.Root as X
import Alpha.Canonical.Common.Format as X
import Alpha.Canonical.Common.Conversions as X

import qualified Data.List as List
import qualified Data.Text as Text

-- | Defines the codomain of the sign function
data Sign = Negative | Neutral | Positive
    deriving (Ord, Eq, Generic, Data, Typeable, Enum)
    
-- | Specifies an orientation. Unlike 'Sign', 'Orientation' cannot be neutral
data Orientation = Leftward | Rightward
    deriving (Ord, Eq, Generic, Data, Typeable, Enum)
    
-- | Represents a positive or negative infinite value
newtype Unbounded a = Unbounded Orientation
    deriving (Eq, Ord, Generic, Data, Typeable)

-- | Represents a left orientation
data LeftOrient a = LeftOrient

-- | Represents a right orientation
data RightOrient a = RightOrient

-- | Characterizes type whose values may be associated with a sign
-- More specifically, characterizes types whose values may be 
-- partitioned into three disjoint subsets, one called 'Negative'
-- one 'Positive' the other 'Neutral'
class Signable a where
    sign::a -> Sign

-- | Advertises the orientation of a construct which may be one
-- of two orientations
class Oriented a where
    orientation::a -> Orientation

-- | Classifies signed integral types    
class (Signable i, Integral i) => SignedIntegral i where

class (Unsignable i, Integral i) => UnsignedIntegral i where

-- | Classifies types with which a sign cannot be associated
class Unsignable a where

class (Bounded a, Integral a) => BoundedIntegral a where    

-- Produces a 'Sign' of positive polarity
positive::Sign
positive = Positive

-- Produces a 'Sign' of negative polarity
negative::Sign
negative = Negative

-- Produces a 'Sign' of neutral polarity
neutral::Sign
neutral = Neutral

-- Produces a left 'Orientation'
leftward::Orientation
leftward = Leftward

-- Produces a right 'Orientation'
rightward::Orientation
rightward = Rightward

-- Constructs an infinite value
unbounded::Orientation -> Unbounded a
unbounded = Unbounded

sign'::(Num a,Ord a) => a -> Sign
sign' a | lt' a 0 = Negative
        | gt' a 0 = Positive
        | a == 0 = Neutral

instance ToInt Sign where
    int Positive = 1
    int Negative = -1
    int Neutral = 0        
        
-- *Oriented instances
-------------------------------------------------------------------------------            
instance Oriented (Unbounded a) where
    orientation (Unbounded o) = o


instance Oriented Sign where
    orientation Negative = Leftward
    orientation _ = Rightward
        
-- *Formattable instances    
-------------------------------------------------------------------------------
instance Formattable (Unbounded a)  where
    format (Unbounded Leftward) = Dash <> " inf"
    format _ = Plus <> " inf"
    
instance Formattable Sign  where
    format Positive = Plus
    format Negative = Dash
    format Neutral = Blank

instance Show Sign where
    show = string . format

        
instance Show (Unbounded a) where
    show = Text.unpack . format

-- *UnsignedIntegral instances    
-------------------------------------------------------------------------------    
instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64
instance UnsignedIntegral Natural

-- *BoundedIntegral instances    
-------------------------------------------------------------------------------    
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

-- *Unsignable instances    
-------------------------------------------------------------------------------    
instance Unsignable Natural
instance Unsignable Word
instance Unsignable Word8
instance Unsignable Word16
instance Unsignable Word32
instance Unsignable Word64

-- *SignedIntegral instances    
-------------------------------------------------------------------------------    
instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64
instance SignedIntegral Integer        

-- *Signable instances    
-------------------------------------------------------------------------------    

instance Signable (Unbounded a) where
    sign (Unbounded Leftward) = Negative
    sign _ = Positive


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
        