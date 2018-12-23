-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Exponential
(
    Exponential(..), exponential,
    Powered(..),    
    Raised(..),
    

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Multiplicative
import qualified Data.List as List

-- | Represents a family of types that support a notion of (potentially) heterogenous 
-- exponentiation
type family Raised b p 
type instance Raised b Natural = b
type instance Raised b Word = b
type instance Raised b Word8 = b
type instance Raised b Word16 = b
type instance Raised b Word32 = b
type instance Raised b Word64 = b

-- | Characterizes a type that can be raised to a natural power
class (Multiplicative b, Unital b) =>  Powered b where

    pow::b -> Natural -> Raised b Natural
    pow b p = product where
         factors = List.replicate (fromIntegral p) b
         product = reduce one (*) factors
    {-# INLINE pow #-}

    -- | Infix synonym for 'pow'
    (^)::b -> Natural -> b
    (^) = pow
    {-# INLINE (^) #-}
    infixr 8 ^

-- | Represents a base raised to a power prior to evaluation
newtype Exponential b p = Exponential (b, p)
    deriving (Eq,Ord,Generic)
instance Newtype (Exponential b p)    

-- | Constructs, but does not evaluate, an exponential representation
exponential::b -> p -> Exponential b p
exponential b p = Exponential (b,p)

instance (Powered b) => Computable (Exponential b Natural) where
    type Computed (Exponential b Natural) = b
    compute(Exponential (b,p)) = b ^ p

-- Powered (Naturally)
-------------------------------------------------------------------------------
instance Powered Natural where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Integer where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Int where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Int8 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Int16 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Int32 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Int64 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Word where 
    pow = pow'
    {-# INLINE pow #-}
instance  Powered Word8 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Word16 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Word32 where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Word64 where 
    pow = pow'
    {-# INLINE pow #-}
instance (Integral n) => Powered (Ratio n) where 
    pow = pow'
    {-# INLINE pow #-}    
instance Powered Float where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered Double where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered CFloat where 
    pow = pow'
    {-# INLINE pow #-}
instance Powered CDouble where 
    pow = pow'
    {-# INLINE pow #-}
