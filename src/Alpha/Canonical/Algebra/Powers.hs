-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Powers
(
    module X,
    Power(..),    
    IntegralPower(..),
    FloatingPower(..),
    Exponential(..),
)
where

import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Multiplicative as X
import Alpha.Canonical.Algebra.Subtractive as X
import Alpha.Canonical.Algebra.Additive as X
import Alpha.Canonical.Algebra.Divisive as X
import Alpha.Canonical.Algebra.Distributive as X

import qualified Data.List as List

-- | Represents a base raised to a power prior to evaluation
newtype Exponential b p = Exponential (b, p)
    deriving (Eq,Ord,Generic)
instance Newtype (Exponential b p)    
    
-- | Characterizes a type that can be Raised to a natural (nonnegative) power
class (Multiplicative b, Unital b) =>  Power b where
    pow::b -> Natural -> b
    pow b p = product where
         factors = List.replicate (integral p) b
         product = reduce one (*) factors
    {-# INLINE pow #-}

    -- | Infix synonym for 'pow'
    (^)::b -> Natural -> b
    (^) = pow
    {-# INLINE (^) #-}
    infixr 8 ^

-- | Characterizes a type that can be Raised to an integral power
class Fractional a => IntegralPower a where
    powi::(Integral p) => a -> p -> a

    (^^)::(Integral p) => a -> p -> a
    (^^) = powi
    {-# INLINE (^^) #-}
    infixr 8 ^^

-- | Characterizes a type that can be Raised to a floating power    
class Floating a => FloatingPower a where
    powa::a -> a -> a

    (**)::a -> a -> a
    (**) = powa
    {-# INLINE (**) #-}
    infixr 8 **

--------------------------------------------------------------------------------
-- * Power Instances
--------------------------------------------------------------------------------
instance Power Natural where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Integer where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Int where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Int8 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Int16 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Int32 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Int64 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Word where 
    pow = pow'
    {-# INLINE pow #-}
instance  Power Word8 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Word16 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Word32 where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Word64 where 
    pow = pow'
    {-# INLINE pow #-}
instance (Integral n) => Power (Ratio n) where 
    pow = pow'
    {-# INLINE pow #-}    
instance Power Float where 
    pow = pow'
    {-# INLINE pow #-}
instance Power Double where 
    pow = pow'
    {-# INLINE pow #-}
instance Power CFloat where 
    pow = pow'
    {-# INLINE pow #-}
instance Power CDouble where 
    pow = pow'
    {-# INLINE pow #-}

--------------------------------------------------------------------------------
-- * FloatingPower Instances
--------------------------------------------------------------------------------
instance FloatingPower Float where 
    powa = powa'
    {-# INLINE powa #-}
instance FloatingPower Double where 
    powa = powa'
    {-# INLINE powa #-}
instance FloatingPower CFloat where 
    powa = powa'
    {-# INLINE powa #-}
instance FloatingPower CDouble where 
    powa = powa'
    {-# INLINE powa #-}

--------------------------------------------------------------------------------
-- * IntegralPower Instances
--------------------------------------------------------------------------------
instance (Integral n) => IntegralPower (Ratio n) where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegralPower Float where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegralPower Double where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegralPower CFloat where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegralPower CDouble where 
    powi = pow''
    {-# INLINE powi #-}    