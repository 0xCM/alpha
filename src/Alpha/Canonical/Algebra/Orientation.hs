module Alpha.Canonical.Algebra.Orientation
(    
    Sign(..),     
    Signable(..), 
    Unsignable(..),
    Signed(..),
    Unsigned(..),
    Reversible(..),
    Transposable(..),
    Flippable(..),
    Negatable(..),
    positive, negative, neutral,
    
) where
import Alpha.Base
import Data.Ord
import Alpha.Native
import Alpha.Canonical.Operators

import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Text as Text


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

class Flippable a where
    type Flipped a    
    flip::a -> Flipped a

-- / Characterizes types for which unary negation may be defined
class Negatable a where
    type Negated a
    type Negated a = a

    -- | Negates the operand
    negate::a -> Negated a

-- | Characterizes a type that manifests the concept
-- of an invertible reversion    
class Reversible a b | a -> b, b -> a  where
    reverse::a -> b

-- Characterizes structures that support a notion of duality such that
-- transpose . transpose = id
-- Canonical examples are vectors and matrices
class Transposable a where
    type Transposed a
    type Transposed a = a

    transpose::a -> Transposed a
    
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

instance Reversible [a] [a] where
    reverse = List.reverse
    
instance Reversible Text Text where    
    reverse = Text.reverse
    
instance Transposable [a] where
    transpose = reverse
    
instance (Ord a, Ord b) => Flippable (Map a b) where
    type Flipped (Map a b) = Map b a
    flip m = Map.toList m |> fmap (\(y,z) -> (z,y)) |> Map.fromList
    
instance Flippable (a -> b -> c) where
    type Flipped (a -> b -> c) = b -> a -> c
    flip = flip'
        
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
